library(tidyverse)
library(glue)
library(lubridate)

sw = suppressWarnings
sm = suppressMessages

# inputs ####

earliest_year = 2010

# setup ####

source('src/00_globals.R')

dir.create('data/facilities_with_missing_location_info', showWarnings = FALSE)

# identify NEI facilities without lat/long ####

nei = read_csv('data/nei/nei_joined.csv', col_types = cols(.default = 'c'))
nei_loc_missing = filter(nei, location_set_to_county_centroid == TRUE) %>%
    select(-LATITUDE, -LONGITUDE)

# identify TRI facilities without lat/long ####

tri = map_dfr(list.files('data/tri/raw/', full.names = TRUE),
              read_csv, col_types = cols(.default = 'c'))

tri_loc_missing = tri %>%
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
    rename(year = REPORTING_YEAR, state = STATE_ABBR, county = COUNTY_NAME) %>%
    mutate(across(all_of(c('year', 'load_kg', 'lat', 'lon')),
                  as.numeric)) %>%
    filter(! is.na(load_kg) & load_kg > 0) %>%
    filter(year >= earliest_year) %>%
    filter(is.na(lat)) %>%
    select(-lat, -lon, -FAC_LATITUDE, -FAC_LONGITUDE)

#write ####

write_csv(nei_loc_missing, 'data/facilities_with_missing_location_info/nei.csv')
write_csv(tri_loc_missing, 'data/facilities_with_missing_location_info/tri.csv')
