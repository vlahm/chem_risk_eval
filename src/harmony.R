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
           lat = `Facility Latitude`, lon = `Facility Longitude`) %>%
           # `% Load from Limits`) %>%
    mutate(across(all_of(c('year', 'load_kg', 'lat', 'lon')),#, '% Load from Limits')),
                  as.numeric)) %>%
    filter(! is.na(load_kg) & load_kg > 0) %>%
    filter(year >= earliest_year) %>%
    left_join(county_centroids, by = c('state', 'county')) %>%
    mutate(location_set_to_county_centroid = ifelse(is.na(lat), TRUE, FALSE),
           lat = ifelse(is.na(lat), cty_lat, lat),
           lon = ifelse(is.na(lon), cty_lon, lon)) %>%
    select(-cty_lat, -cty_lon) %>%
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
    mutate(load_kg = as.numeric(EMISSIONS) / 2.2046) %>% # lbs to kg
    select(year = INVENTORY_YEAR, state = STATE, county = COUNTY,
           cas = POLLUTANT_CODE, load_kg,
           lat = LATITUDE, lon = LONGITUDE, location_set_to_county_centroid) %>%
           # `% Load from Limits`) %>%
    mutate(across(all_of(c('year', 'load_kg', 'lat', 'lon')),#, '% Load from Limits')),
                  as.numeric)) %>%
    filter(! is.na(load_kg) & load_kg > 0) %>%
    filter(year >= earliest_year) %>%
    mutate(source = 'NEI')


