library(tidyverse)
library(glue)
library(lubridate)

sw = suppressWarnings
sm = suppressMessages

#TODO: convert inputs to command line args?

# inputs ####

earliest_year = 2010

# setup ####

source('src/globals.R')

cas = read_csv('data/general/target_substances.csv', col_types=cols())

cities = read_csv('data/general/cities.csv', col_types=cols())

# houston_counties = cities %>%
#     filter(! is.na(city) & city == 'HOUSTON') %>%
#     pull(county)

county_centroids = cities %>%
    select(state, county, cty_lat = lat, cty_lon = lon) %>%
    mutate(county = clean_county_names(county))

# harmonize DMR data ####

dmr = map_dfr(list.files('data/dmr', full.names = TRUE),
             read_csv, col_types = cols(.default = 'c'))

dmr = dmr %>%
    select(year = Year, state = State, county = County,
           cas = `CAS Number`, load_kg = `Pollutant Load (kg/yr)`,
           lat = `Facility Latitude`, lon = `Facility Longitude`,
           `FRS ID`) %>%
           # `% Load from Limits`) %>%
    mutate(across(all_of(c('year', 'load_kg', 'lat', 'lon')),#, '% Load from Limits')),
                  as.numeric)) %>%
    filter(! is.na(load_kg) & load_kg > 0) %>%
    filter(year >= earliest_year) %>%
    mutate(medium = NA) %>%
    group_by(year, state, county, cas, `FRS ID`, medium) %>%
    summarize(load_kg = sum(load_kg),
              lat = first(lat),
              lon = first(lon),
              .groups = 'drop') %>%
    left_join(county_centroids, by = c('state', 'county')) %>%
    mutate(location_set_to_county_centroid = ifelse(is.na(lat), TRUE, FALSE),
           lat = ifelse(is.na(lat), cty_lat, lat),
           lon = ifelse(is.na(lon), cty_lon, lon)) %>%
    select(-cty_lat, -cty_lon, -`FRS ID`) %>%
    mutate(source = 'DMR')

# maxes_by_cas = d %>%
#     group_by(cas) %>%
#     summarize(load_max = max(load_kg, na.rm=TRUE)) %>%
#     ungroup()
#
# d = d %>%
#     left_join(maxes_by_cas, by = 'cas') %>%
#     mutate(load_pct_max = load_kg / load_max,
#            load_pct_bin = base::cut(load_pct_max,
#                                     breaks = 10),
#            load_pct_bin = as.character(load_pct_bin),
#            load_pct_bin = ifelse(load_pct_bin == '(-0.001,0.1]', '(0,0.1]', load_pct_bin),
#            row_dupe_factor = as.numeric(str_match(load_pct_bin, ',([0-9\\.]+)]$')[, 2]) * 10)
#
# d = d[rep(row.names(d), times = d$row_dupe_factor), ]


# harmonize NEI data ####

nei = read_csv('data/nei/nei_joined.csv', col_types = cols(.default = 'c'))

nei = nei %>%
    mutate(load_kg = as.numeric(EMISSIONS) / 2.2046,
           medium = NA) %>% # lbs to kg
    select(year = INVENTORY_YEAR, state = STATE, county = COUNTY,
           cas = POLLUTANT_CODE, medium, load_kg, lat = LATITUDE, lon = LONGITUDE,
           location_set_to_county_centroid, EIS_FACILITY_ID) %>%
    mutate(across(all_of(c('year', 'load_kg', 'lat', 'lon')),
                  as.numeric)) %>%
    filter(! is.na(load_kg) & load_kg > 0) %>%
    filter(year >= earliest_year) %>%
    group_by(year, state, county, cas, EIS_FACILITY_ID, medium) %>%
    summarize(load_kg = sum(load_kg),
              lat = first(lat),
              lon = first(lon),
              location_set_to_county_centroid = ! any(! as.logical(location_set_to_county_centroid)),
              .groups = 'drop') %>%
    select(-EIS_FACILITY_ID) %>%
    mutate(source = 'NEI')


# harmonize TRI data ####

tri = map_dfr(list.files('data/tri/raw/', full.names = TRUE),
              read_csv, col_types = cols(.default = 'c'))

#look for NAs in facility IDs. if they exist, gotta group by some other facil identifier

tri = tri %>%
    mutate(lat = ifelse(FAC_LATITUDE == '0', NA, FAC_LATITUDE),
           lon = ifelse(FAC_LONGITUDE == '0', NA, FAC_LONGITUDE)) %>%
    filter(! is.na(CAS_REGISTRY_NUMBER)) %>%
    mutate(load_kg = as.numeric(TOTAL_RELEASE) / 2.2046, # lbs to kg
           cas = gsub('-', '', CAS_REGISTRY_NUMBER),
           medium = case_when(grepl('AIR', ENVIRONMENTAL_MEDIUM) ~ 'air',
                              grepl('WATER', ENVIRONMENTAL_MEDIUM) ~ 'water',
                              grepl('LANDF|UNINJ', ENVIRONMENTAL_MEDIUM) ~ 'ground',
                              TRUE ~ 'surface')) %>%
    select(year = REPORTING_YEAR, state = STATE_ABBR, county = COUNTY_NAME,
           cas, load_kg, medium, lat, lon, TRI_FACILITY_ID) %>%
    mutate(across(all_of(c('year', 'load_kg', 'lat', 'lon')),
                  as.numeric)) %>%
    filter(! is.na(load_kg) & load_kg > 0) %>%
    filter(year >= earliest_year) %>%
    # filter(year == 1987, state == 'KY', county=='JEFFERSON', cas=='106990', TRI_FACILITY_ID=='40211BFGDR4100B')
    group_by(year, state, county, cas, TRI_FACILITY_ID, medium) %>%
    summarize(load_kg = sum(load_kg),
              lat = first(lat),
              lon = first(lon),
              .groups = 'drop') %>%
    left_join(county_centroids, by = c('state', 'county')) %>%
    mutate(location_set_to_county_centroid = ifelse(is.na(lat), TRUE, FALSE),
           lat = ifelse(is.na(lat), cty_lat, dms_to_decdeg(lat)),
           lon = ifelse(is.na(lon), cty_lon, dms_to_decdeg(lon)),
           lon = -abs(lon)) %>%
    select(-cty_lat, -cty_lon, -TRI_FACILITY_ID) %>%
    mutate(source = 'TRI')



# combine ####

out = bind_rows(dmr, nei) %>%
    bind_rows(tri)

write_csv(out, 'data/emissions_harmonized.csv')
