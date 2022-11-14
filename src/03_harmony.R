library(tidyverse)
library(glue)
library(lubridate)

sw = suppressWarnings
sm = suppressMessages

#TODO: convert inputs to command line args?

# inputs ####

earliest_year = 2010

# setup ####

source('src/00_globals.R')

cas = read_csv('data/general/target_substances.csv', col_types=cols())

cities = read_csv('data/general/cities.csv', col_types=cols()) %>%
    mutate(county = clean_county_names(county))

houston_counties = cities %>%
    filter(! is.na(city) & city == 'HOUSTON') %>%
    pull(county)

county_centroids = cities %>%
    select(state, county, cty_lat = lat, cty_lon = lon)

# harmonize DMR data (custom way; OBSOLETE) ####

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
    select(-cty_lat, -cty_lon) %>%
    mutate(source = 'DMR',
           illegal = FALSE)

# dmr %>%
#     filter(state == 'KY', county == 'JEFFERSON') %>%
#     group_by(cas, year) %>%
#     summarize(load_kg = sum(load_kg)) %>%
#     print(n=400)

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


# harmonize NEI data (custom way; OBSOLETE) ####

nei = read_csv('data/nei/nei_joined.csv', col_types = cols(.default = 'c'))

nei = nei %>%
    distinct(POLLUTANT_CODE, INVENTORY_YEAR, SITE_NAME, EMISSIONS,
             STATE, COUNTY, EIS_FACILITY_ID, .keep_all = TRUE) %>%
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
    mutate(source = 'NEI',
           illegal = FALSE)

nei %>%
    filter(state == 'LA') %>%
    distinct(EIS_FACILITY_ID) %>%
    write_csv('../stewi_comparison/nei_facilities_canceralley.csv')

nei %>%
    filter(state == 'LA', year == 2017, cas=='75343') %>%
    summarize(load_kg = sum(load_kg))

# nei %>%
#     group_by(cas, year) %>%
#     summarize(load_kg = mean(load_kg))

# harmonize TRI data (custom way; OBSOLETE) ####

tri = map_dfr(list.files('data/tri/raw/', full.names = TRUE),
              read_csv, col_types = cols(.default = 'c'))

#look for NAs in facility IDs. if they exist, gotta group by some other facil identifier

tri = tri %>%
    distinct(CAS_REGISTRY_NUMBER, ENVIRONMENTAL_MEDIUM, REPORTING_YEAR,
             STATE_ABBR, COUNTY_NAME, TRI_FACILITY_ID, DOC_CTRL_NUM,
             .keep_all = TRUE) %>%
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
           cas, load_kg, medium, lat, lon) %>%
    mutate(across(all_of(c('year', 'load_kg', 'lat', 'lon')),
                  as.numeric)) %>%
    filter(! is.na(load_kg) & load_kg > 0) %>%
    filter(year >= earliest_year) %>%
    # filter(year == 1987, state == 'KY', county=='JEFFERSON', cas=='106990', TRI_FACILITY_ID=='40211BFGDR4100B')
    group_by(year, state, county, cas, lat, lon, medium) %>%
    summarize(load_kg = sum(load_kg),
              lat = first(lat),
              lon = first(lon),
              .groups = 'drop') %>%
    left_join(county_centroids, by = c('state', 'county')) %>%
    mutate(location_set_to_county_centroid = ifelse(is.na(lat), TRUE, FALSE),
           lat = ifelse(is.na(lat), cty_lat, dms_to_decdeg(lat)),
           lon = ifelse(is.na(lon), cty_lon, dms_to_decdeg(lon)),
           lon = -abs(lon)) %>%
    select(-cty_lat, -cty_lon) %>%
    mutate(source = 'TRI',
           illegal = FALSE)

# write_csv(filter(tri, state=='LA') %>% select(TRI_FACILITY_ID), '../stewi_comparison/tri_facilities_canceralley.csv')

filter(tri, state=='LA', year == '2017') %>% summarize(sumkg = sum(load_kg, na.rm=T))

# tri %>%
#     # filter(state == 'KY', county == 'JEFFERSON') %>%
#     filter(state == 'LA', year == '2019') %>%
#     group_by(cas, year) %>%
#     summarize(load_kg = sum(load_kg)) %>%
#     print(n=100)


# harmonize NPDES violation data ####

npdes = read_csv('data/echo/npdes_violations.csv', col_types = cols(.default = 'c'))

#look for NAs in facility IDs. if they exist, gotta group by some other facil identifier


npdes_loads_concs = npdes %>%
    filter(grepl('numeric violation$', VIOLATION_DESC, ignore.case = TRUE)) %>%
    mutate(year = year(mdy(VALUE_RECEIVED_DATE))) %>%
           # state = substr(NPDES_ID, 1, 2)) %>%
    select(year, state = State, county = County,
           cas = `CAS Number`,
           STANDARD_UNIT_DESC, DMR_VALUE_STANDARD_UNITS, STATISTICAL_BASE_SHORT_DESC,
           lat = `Facility Latitude`, lon = `Facility Longitude`, FRS_ID) %>%
    mutate(across(all_of(c('year', 'DMR_VALUE_STANDARD_UNITS', 'lat', 'lon')),
                  as.numeric))

npdes_loads = npdes_loads_concs %>%
    filter(STANDARD_UNIT_DESC == 'kg/d') %>%
    mutate(DMR_VALUE_STANDARD_UNITS = ifelse(grepl('^MO ', STATISTICAL_BASE_SHORT_DESC),
                                             DMR_VALUE_STANDARD_UNITS * 30,
                                             DMR_VALUE_STANDARD_UNITS)) %>%
    rename(load_kg = DMR_VALUE_STANDARD_UNITS) %>%
    select(-STANDARD_UNIT_DESC, -STATISTICAL_BASE_SHORT_DESC) %>%
    filter(year >= earliest_year) %>%
    group_by(year, state, county, cas, FRS_ID) %>%
    summarize(load_kg = sum(load_kg),
              lat = first(lat),
              lon = first(lon),
              .groups = 'drop') %>%
    mutate(source = 'NPDES',
           medium = NA,
           location_set_to_county_centroid = FALSE,
           illegal = TRUE) %>%
    select(year, state, county, cas, medium, load_kg, lat, lon,
           location_set_to_county_centroid, source, illegal, FRS_ID)

npdes_concs = npdes_loads_concs %>%
    filter(STANDARD_UNIT_DESC == 'mg/L') %>%
    filter(year >= earliest_year) %>%
    # group_by(year, state, county, cas, FRS_ID) %>%
    # summarize(concentration_mgL = sum(DMR_VALUE_STANDARD_UNITS),
    #           lat = first(lat),
    #           lon = first(lon),
    #           .groups = 'drop')
    mutate(source = 'NPDES',
           medium = NA,
           location_set_to_county_centroid = FALSE,
           illegal = TRUE) %>%
    select(year, state, county, cas, medium,
           concentration_mgL = DMR_VALUE_STANDARD_UNITS, STATISTICAL_BASE_SHORT_DESC,
           lat, lon,
           location_set_to_county_centroid, source, illegal)

# load DMR, NEI, TRI data (retrieved via stewi) ####

dmr = read_csv('data/stewi_data_dmr_joined2.csv') %>%
    mutate(cas = gsub('-', '', cas),
           frs_id = as.character(frs_id))

nei = read_csv('data/stewi_data_nei_joined2.csv') %>%
    mutate(cas = gsub('-', '', cas),
           frs_id = as.character(frs_id))

tri = read_csv('data/stewi_data_tri_joined2.csv') %>%
    mutate(cas = gsub('-', '', cas),
           frs_id = as.character(frs_id))

npdes_loads = rename(npdes_loads, frs_id = FRS_ID)

# ONE-TIME-ONLY load DMR, NEI, TRI data (retrieved via stewi, NEI-DMR priority) ####
#
# dmr = read_csv('data/stewi_data_dmr_joined_dmrNeiPriority.csv') %>%
#     mutate(cas = gsub('-', '', cas),
#            frs_id = as.character(frs_id))
#
# nei = read_csv('data/stewi_data_nei_joined_dmrNeiPriority.csv') %>%
#     mutate(cas = gsub('-', '', cas),
#            frs_id = as.character(frs_id))
#
# tri = read_csv('data/stewi_data_tri_joined_dmrNeiPriority.csv') %>%
#     mutate(cas = gsub('-', '', cas),
#            frs_id = as.character(frs_id))
#
# npdes_loads = rename(npdes_loads, frs_id = FRS_ID)

# correct DMR data to avoid double-counting of effluent exceedances also reported to NPDES ####

# multireports = npdes_loads %>%
#     rename(frs_id = FRS_ID) %>%
#     left_join(dmr, by=c('year', 'state', 'county', 'cas', 'frs_id')) %>%
#     select(year, state, county, cas, frs_id,
#            multirp_amt = load_kg.x, dmr_total = load_kg.y) %>%
#     filter(! is.na(dmr_total)) %>%
#     select(-dmr_total)

# dmr = dmr %>%
#     left_join(multireports, by = c('year', 'state', 'county', 'cas', 'frs_id')) %>%
#     mutate(load_kg = ifelse(! is.na(multirp_amt), load_kg - multirp_amt, load_kg)) %>%
#     select(-multirp_amt)

# combine ####

out = bind_rows(dmr, nei) %>%
    bind_rows(tri) %>%
    bind_rows(npdes_loads) %>%
    mutate(target_location = case_when(state == 'LA' ~ 'Cancer Alley',
                                       state == 'KY' ~ 'Louisville',
                                       county %in% houston_counties ~ 'Houston',
                                       TRUE ~ 'Port Arthur')) %>%
    distinct(year, state, county, cas, medium, frs_id, source,
             .keep_all = TRUE)

# duplicated(select(out, year, state, county, cas, medium, lat, lon, source))

# excess = out %>%
#     filter(location_set_to_county_centroid) %>%
#     group_by(year, target_location, cas, medium, source) %>%
#     summarize(load_kg_excess = sum(load_kg),
#               .groups = 'drop')
#
# out_avg_load_distributed = out %>%
#     filter(! location_set_to_county_centroid) %>%
#     group_by(year, target_location, cas, medium, source) %>%
#     mutate(n = n()) %>%
#     left_join(excess) %>%
#     mutate(
#         load_kg_excess = ifelse(is.na(load_kg_excess), 0, load_kg_excess),
#         load_kg = load_kg + (load_kg_excess / n)) %>%
#     ungroup() %>%
#     select(-location_set_to_county_centroid, -n, -load_kg_excess)

# write_csv(out, 'data/emissions_harmonized_epamethod_NEIDMRpriority_2010-22.csv')
write_csv(out, 'data/emissions_harmonized_epamethod_TRIpriority_2010-22.csv')
# write_csv(out, 'data/emissions_harmonized_excess_assigned_to_cty_centroid_2010-22.csv')
# write_csv(out_avg_load_distributed, 'data/emissions_harmonized_excess_distributed_evenly_2010-22.csv')
