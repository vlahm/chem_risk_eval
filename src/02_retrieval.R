library(tidyverse)
library(httr)
library(glue)
library(feather)
library(mapview)
library(lubridate)

mv = mapview::mapview
sw = suppressWarnings
sm = suppressMessages

setwd('earthjustice/chem_risk_eval/')

## 0 - setup ####

source('src/00_globals.R')

cas = read_csv('data/general/target_substances.csv', col_types=cols())

cities = read_csv('data/general/cities.csv', col_types=cols())

## 1 - [ABANDONED] ICIS-AIR: envirofacts REST service ####

# icis_air_chem = tibble()
# for(i in 10:nrow(cas)){
for(i in seq_len(nrow(cas))){

    srs_id = cas$SRS_id[i]

    # icis_air_chem_ = query_envirofacts(
    #     table_names=c('ICIS_DMR_FORM_PARAMETER', 'ICIS_ENF_CONCL_POLLUTANT',
    #                   'ICIS_LIMIT', 'REF_POLLUTANT', 'TRI_CHEM_INFO'),
    #                   # icis_air_chem = query_envirofacts(table_names=c('ICIS_LIMIT'),
    #     filter_column='SRS_ID',
    #     filter_operator='=',
    #     filter_value=srs_id,
    #     join_column='SRS_ID',
    #     # save_intermediates=TRUE,
    #     warn=FALSE)

    redundant_chem_cols_1 = c('POLLUTANT_DESC', 'CHEMICAL_FORMULA', 'CHEMICAL_ABSTRACT_SERVICE_NMBR',
                              'EPA_ID', 'SRS_SYSTEMATIC_NAME')
    redundant_chem_cols_2 = c('MONITORING_LOCATION_DESC',
                              'PARAMETER_DESC', 'PERCENT_EXCEEDENCE_FLAG', 'PERCENT_REMOVAL_FLAG',
                              'POLLUTANT_CATEGORY_DESC')
    # joining_key_cols = c('SRS_ID', 'LIMIT_ID')

    pollutants = query_ef_table(table_name=c('REF_POLLUTANT'), column_name='SRS_ID', operator='=', column_value=as.character(srs_id), warn=TRUE)
    if(nrow(pollutants)){
        pollutants = pollutants %>%
            select(SRS_ID, POLLUTANT_CODE, STATUS_FLAG, #flag indicates whether the pollutant record is active or inactive
                   all_of(redundant_chem_cols_1)) %>%
            rename_with(~paste0(., '_pol'), -ends_with('_ID'))
    }

    # limits = query_ef_table(table_name=c('ICIS_LIMIT'), column_name='SRS_ID', operator='=', column_value=as.character(srs_id), warn=TRUE) %>%
    #     select(-any_of(redundant_chem_cols_1), all_of(redundant_chem_cols_2)) %>%
    #     rename_with(~paste0(., '_lim'), -ends_with('_ID'))

    #NO DATA FOR ANY CAS
    # emissions = query_ef_table(table_name=c('EMISSIONS'), column_name='POLLUTANT_CODE', operator='=', column_value=as.character(pollutants$POLLUTANT_CODE_pol), warn=TRUE)

#     if(! 'POLLUTANT_CODE_pol' %in% colnames(pollutants)) next
#     # emiss_sec = query_ef_table(table_name=c('EMISSIONS_SECTOR'), column_name='POLLUTANT_CODE', operator='=', column_value=as.character(pollutants$POLLUTANT_CODE_pol), warn=TRUE)
#     r_ = query_ef_rows(table_name=c('EMISSIONS_SECTOR'), column_name='POLLUTANT_CODE', operator='=', column_value=as.character(pollutants$POLLUTANT_CODE_pol))
#     print(r_)
# }

#     r1 = query_ef_rows(table_name=c('TRI_CHEM_INFO'), column_name='POLLUTANT_CODE', operator='=', column_value=as.character(pollutants$POLLUTANT_CODE_pol))
#     r2 = query_ef_rows(table_name=c('ICIS_ENF_CONCL_POLLUTANT'), column_name='SRS_ID', operator='=', column_value=as.character(srs_id))
#     print(paste(r1, r2))
# }

    # tri_chem = query_ef_table(table_name=c('TRI_CHEM_INFO'), column_name='POLLUTANT_CODE', operator='=', column_value=as.character(pollutants$POLLUTANT_CODE_pol), warn=TRUE)
    # icis_enf = query_ef_table(table_name=c('ICIS_ENF_CONCL_POLLUTANT'), column_name='SRS_ID', operator='=', column_value=as.character(srs_id), warn=TRUE)
    # dmr_forms = query_ef_table(table_name=c('ICIS_DMR_FORM_PARAMETER'), column_name='SRS_ID', operator='=', column_value=as.character(srs_id), warn=TRUE) %>%
    #     select(-any_of(c(redundant_chem_cols_1, redundant_chem_cols_2))) %>%
    #     rename_with(~paste0(., '_dmr'), -ends_with('_ID'))
    #
    # xx = full_join(xx, eee,
    #                by = toupper(join_column))
    # setdiff(colnames(aaa), colnames(eee))
    # setdiff(colnames(eee), colnames(aaa))
    # shared_cols = intersect(colnames(eee), colnames(aaa))
    # filter(eee, LIMIT_ID == '20014704') %>% View()
    # filter(aaa, LIMIT_ID == '20014704') %>% View()

    # left_cols = setdiff(colnames(dmr_forms), colnames(limits))
    chemd = dmr_forms %>%
        # select(SRS_ID, LIMIT_ID, all_of(left_cols)) %>%
        left_join(limits,
                  by = c('SRS_ID', 'LIMIT_ID')) %>%
        left_join(pollutants,
                  by = c('SRS_ID')) %>%
        select(ends_with('_ID'), everything())
        # select(order(colnames(.)))

    #save temporary output in case of error, power failure, etc.
    # saveRDS(icis_air_chem_, paste0('~/temp/icis_air_', srs_id, '.rds'))
    # assign(paste0('icis_air_', srs_id), icis_air_chem_, pos=.GlobalEnv)

    write_feather(chemd, paste0('data/envirofacts_chemtables/icis_air/', srs_id, '.feather'))

    # for(j in unique(chemd$){
    #
    #
    # }

    # icis_air_chem = bind_rows(icis_air_chem, icis_air_chem_)
}

## 2 - TRI: envirofacts REST service ####

for(i in seq_len(nrow(cas))){

    srs_id = cas$SRS_id[i]
    print(paste0(Sys.time(), ' working on chem: ', srs_id, ' (', i, ' of ', nrow(cas), ')'))

    #chemicals
    print(paste(Sys.time(), 'getting chem data'))
    chems = query_ef_table(table_name=c('TRI_CHEM_INFO'), column_name='SRS_ID', operator='=',
                           column_value=as.character(srs_id), warn=TRUE)
    if(nrow(chems) > 1) stop('hm? pick a row.')

    #report forms
    print(paste(Sys.time(), 'getting report data'))
    rep_frms <- try({
        query_ef_table(table_name=c('TRI_REPORTING_FORM'), column_name='TRI_CHEM_ID', operator='=',
                       column_value=chems$TRI_CHEM_ID, warn=TRUE)
    })
    if(inherits(rep_frms, 'try-error')){
        print('no forms available')
        next
    }

    #facilities
    print(paste(Sys.time(), 'getting facility data'))
    facil_fails = c()
    facil = tibble()
    for(j in seq_len(nrow(cities))){

        print(paste('county', j, 'of', nrow(cities)))

        facil_ = tibble()
        city = cities[j, ]
        cnty_srch = is.na(city$city)
        # colq = if(cnty_srch) 'COUNTY_NAME' else 'CITY_NAME'
        colq = if(cnty_srch) 'STATE_ABBR' else 'CITY_NAME'
        colv = if(cnty_srch) city$state else city$city

        tryres = try({
            facil_ = query_ef_table(table_name=c('TRI_FACILITY'), column_name=colq,
                                    operator='=', column_value=colv, warn=TRUE)

            if(nrow(facil_)) {
                if(! cnty_srch){
                    facil_ = filter(facil_, COUNTY_NAME == city$county & STATE_ABBR == city$state)
                } else {
                    # facil_ = filter(facil_, STATE_ABBR == city$state)
                    facil_ = facil_ %>%
                        mutate(COUNTY_NAME = clean_county_names(COUNTY_NAME)) %>%
                        filter(COUNTY_NAME %in% clean_county_names(cities$county[cities$state == colv]))
                }
            }

            facil = bind_rows(facil, facil_)
        })

        if(inherits(tryres, 'try-error')){
            facil_fails = c(facil_fails, j)
            next
        }
    }
    uniq_facils = unique(facil$TRI_FACILITY_ID)

    #filter forms by facility
    rep_frms = filter(rep_frms, TRI_FACILITY_ID %in% uniq_facils)
    if(! nrow(rep_frms)) next
    docnums = unique(rep_frms$DOC_CTRL_NUM)

    #releases
    print(paste(Sys.time(), 'getting release data'))
    release_fails = c()
    releases = tibble()
    for(i in seq_along(docnums)){

        if(i %% 25 == 0) print(paste('got', i, 'of', length(docnums), 'sets'))
        docnum = docnums[i]

        releases_ = tibble()
        tryres = try({
            releases_ = query_ef_table(table_name=c('TRI_RELEASE_QTY'),
                                       column_name='DOC_CTRL_NUM', operator='=',
                                       column_value=docnum, warn=TRUE)
            releases = bind_rows(releases, releases_)
        })

        if(inherits(tryres, 'try-error')){
            release_fails = c(release_fails, docnum)
            next
        }
    }

    # https://enviro.epa.gov/enviro/EF_METADATA_HTML.tri_page?p_column_name=RELEASE_RANGE_CODE
    releases = mutate(releases,
                      TOTAL_RELEASE = case_when(RELEASE_RANGE_CODE == 1 ~ 5.5,
                                                RELEASE_RANGE_CODE == 2 ~ 250,
                                                RELEASE_RANGE_CODE == 3 ~ 255,
                                                RELEASE_RANGE_CODE == 4 ~ 749.5,
                                                RELEASE_RANGE_CODE == 5 ~ 5.5))

    # #environments
    # print(paste(Sys.time(), 'getting environment data'))
    # water_fails = c()
    # water = tibble()
    # for(i in seq_along(docnums)){
    #
    #     if(i %% 25 == 0) print(paste('got', i, 'of', length(docnums), 'sets'))
    #     docnum = docnums[i]
    #
    #     water_ = tibble()
    #     tryres = try({
    #         water_ = query_ef_table(table_name=c('TRI_WATER_STREAM'),
    #                                   column_name='DOC_CTRL_NUM', operator='=',
    #                                   column_value=docnum, warn=TRUE)
    #         water = bind_rows(water, water_)
    #     })
    #
    #     if(inherits(tryres, 'try-error')){
    #         water_fails = c(water_fails, docnum)
    #         next
    #     }
    # }

    #write data
    chemd = left_join(rep_frms, facil, by = 'TRI_FACILITY_ID') %>%
        left_join(chems, by = 'TRI_CHEM_ID') %>%
        left_join(releases, by = 'DOC_CTRL_NUM')
        # left_join(water, by = 'DOC_CTRL_NUM')

    # chemd %>%
    #     group_by(CAS_REGISTRY_NUMBER, ENVIRONMENTAL_MEDIUM, REPORTING_YEAR,
    #              STATE_ABBR, COUNTY_NAME, TRI_FACILITY_ID, DOC_CTRL_NUM) %>%
    #     summarize(across(everything(), function(x) length(unique(x)))) %>%
    #     ungroup() %>%
    #     filter(if_any(c('WATER_SEQUENCE_NUM', 'RELEASE_RANGE_CODE', 'TOTAL_RELEASE',
    #                            'RELEASE_NA', 'RELEASE_BASIS_EST_CODE'),
    #                   ~(. != 1))) %>%
    #     select(all_of(c('CAS_REGISTRY_NUMBER', 'ENVIRONMENTAL_MEDIUM', 'REPORTING_YEAR',
    #              'STATE_ABBR', 'COUNTY_NAME', 'TRI_FACILITY_ID', 'DOC_CTRL_NUM', 'WATER_SEQUENCE_NUM', 'RELEASE_RANGE_CODE', 'TOTAL_RELEASE',
    #                      'RELEASE_NA', 'RELEASE_BASIS_EST_CODE')))->xx
    # chemd %>%
    #     filter(CAS_REGISTRY_NUMBER == '50-00-0',
    #
    #     distinct(CAS_REGISTRY_NUMBER, ENVIRONMENTAL_MEDIUM, REPORTING_YEAR,
    #              STATE_ABBR, COUNTY_NAME, TRI_FACILITY_ID, DOC_CTRL_NUM,
    #              .keep_all = TRUE) %>%
    #     mutate(lat = ifelse(FAC_LATITUDE == '0', NA, FAC_LATITUDE),
    #            lon = ifelse(FAC_LONGITUDE == '0', NA, FAC_LONGITUDE)) %>%
    #     filter(! is.na(CAS_REGISTRY_NUMBER)) %>%
    #     mutate(load_kg = as.numeric(TOTAL_RELEASE) / 2.2046, # lbs to kg
    #            cas = gsub('-', '', CAS_REGISTRY_NUMBER),
    #            medium = case_when(grepl('AIR', ENVIRONMENTAL_MEDIUM) ~ 'air',
    #                               grepl('WATER', ENVIRONMENTAL_MEDIUM) ~ 'water',
    #                               grepl('LANDF|UNINJ', ENVIRONMENTAL_MEDIUM) ~ 'ground',
    #                               TRUE ~ 'surface')) %>%
    #     select(year = REPORTING_YEAR, state = STATE_ABBR, county = COUNTY_NAME,
    #            cas, load_kg, medium, lat, lon, TRI_FACILITY_ID) %>%
    #     mutate(across(all_of(c('year', 'load_kg', 'lat', 'lon')),
    #                   as.numeric)) %>%
    #     filter(! is.na(load_kg) & load_kg > 0) %>%
    #     filter(year >= 2010) %>%
    #
    #     # filter(state == 'KY', county == 'JEFFERSON') %>%
    #     filter(state == 'LA', year == '2019') %>%
    #     group_by(cas, year) %>%
    #     summarize(load_kg = mean(load_kg)) %>%
    #     print(n=100)

    write_csv(chemd, paste0('data/tri/raw/', cas$CASRN[i], '.csv'))

    # #write map
    # site_locs = chemd %>%
    #     select(lon=FAC_LONGITUDE, lat=FAC_LATITUDE, everything()) %>%
    #     # distinct() %>%
    #     filter(! is.na(lon) & ! is.na(lat)) %>%
    #     mutate(lon = -sw(dms_to_decdeg(lon)),
    #            lat = sw(dms_to_decdeg(lat))) %>%
    #     filter(! is.na(lon) & ! is.na(lat)) %>%
    #     sf::st_as_sf(coords=c('lon', 'lat'),
    #                  crs = 4326)
    #
    # sites_all = mv(site_locs)
    # sites_with_releases = filter(site_locs, ! is.na(TOTAL_RELEASE))
    #
    # if(nrow(sites_with_releases)){
    #     sites_with_releases = mv(sites_with_releases, color = 'red')
    # } else {
    #     sites_with_releases = NULL
    # }
    #
    # mapshot(sites_all + sites_with_releases,
    #         url = paste0('figs/tri_', cas$CASRN[i], '.html'))
}


## 3 - NEI: envirofacts REST service ####

facil_fails = c()
facil = tibble()
for(i in seq_len(nrow(cas))){

    cas_ = cas$CASRN_nohyphens[i]
    print(paste('working on CAS:', cas_))

    #query facility summary table
    for(stt in unique(cities$state)){

        facil_ = tibble()

        tryres = try({
            facil_ = query_ef_table(table_name='FACILITY_SUMMARY',
                                    column_name=c('STATE', 'POLLUTANT_CODE'),
                                    operator=c('=', '='),
                                    column_value=c(stt, cas_),
                                    warn=FALSE)

            if(nrow(facil_)){
                print(paste('got', nrow(facil_), 'rows for', stt, cas_))

                facil = facil_ %>%
                    mutate(COUNTY = clean_county_names(COUNTY)) %>%
                    filter(COUNTY %in% clean_county_names(cities$county[cities$state == stt])) %>%
                    bind_rows(facil)
            }
        })

        if(inherits(tryres, 'try-error')){
            facil_fails = c(facil_fails, paste(i, stt))
            next
        }
    }
}

# write_csv(facil, 'data/nei/facility_summary.csv')
# facil =  read_csv('data/nei/facility_summary.csv', col_types=cols(.default = 'c'))

#supplement with facility lat/longs
locations = query_ef_table(table_name='FACILITIES', warn=TRUE, verbose=TRUE, chunk_size=1e4)
# write_csv(locations, 'data/nei/locations.csv')
# locations =  read_csv('data/nei/locations.csv', col_types = cols(.default = 'c'))

release_pts = query_ef_table(table_name='RELEASE_POINTS', warn=TRUE, verbose=TRUE, chunk_size=1e4)
# write_csv(release_pts, 'data/nei/release_points.csv')
# release_pts = read_csv('data/nei/release_points.csv', col_types = cols(.default = 'c'))

#bind facility latlong and release point latlong to facility summary table.
#if release point latlong missing, use facil latlong. if both missing, use
#county centroid
nei = locations %>%
    select(EIS_FACILITY_ID, FACILITY_ID, facil_lat = LATITUDE, facil_lon = LONGITUDE) %>%
    right_join(facil, by = 'EIS_FACILITY_ID') %>%
    filter(! is.na(EMISSIONS) & EMISSIONS > 0) %>%
    left_join(select(release_pts, FACILITY_ID, LATITUDE, LONGITUDE),
              by = 'FACILITY_ID') %>%
    mutate(LATITUDE = ifelse(is.na(LATITUDE), facil_lat, LATITUDE),
           LONGITUDE = ifelse(is.na(LONGITUDE), facil_lon, LONGITUDE),
           COUNTY = clean_county_names(COUNTY)) %>%
    left_join(select(cities, STATE = state, COUNTY = county, lat, lon) %>%
                  mutate(COUNTY = clean_county_names(COUNTY)),
              by = c('STATE', 'COUNTY'))

nei$location_set_to_county_centroid = FALSE
nei$location_set_to_county_centroid[is.na(nei$LATITUDE)] = TRUE

nei = nei %>%
    mutate(LATITUDE = ifelse(is.na(LATITUDE), lat, LATITUDE),
           LONGITUDE = ifelse(is.na(LONGITUDE), lon, LONGITUDE)) %>%
    select(-facil_lat, -facil_lon, -lat, -lon)

# nei %>%
#     filter(is.na(LATITUDE),
#            ! is.na(EMISSIONS) & EMISSIONS > 0,
#            INVENTORY_YEAR >= 2012) %>%
#     # select(COMPANY_NAME, SITE_NAME) %>%
#     # print(n=100)
#     distinct(COMPANY_NAME) %>%
#     pull()
#     # {table(.$INVENTORY_YEAR)}

write_csv(nei, 'data/nei/nei_joined.csv')

## 4 - DMR: REST service ####

dmr_fail = c()
for(i in seq_len(nrow(cas))){

    dmrd = tibble()
    casrn = cas$CASRN_nohyphens[i]
    print(paste('working on chem:', casrn))

    for(stt in unique(cities$state)){

        for(yr in as.character(2007:year(Sys.Date()))){

            query_base = 'https://echodata.epa.gov/echo/dmr_rest_services.get_custom_data_annual'
            cas_spec = paste0('?output=CSV&p_cas=', casrn)
            query_mid = '&p_est=Y&p_loads_data=DMR&p_nd=ZERO&p_nutrient_agg=N&p_param_group=N'
            state_spec = paste0('&p_st=', stt)
            year_spec = paste0('&p_year=', yr)

            query = paste0(query_base, cas_spec, query_mid, state_spec, year_spec)

            tryresp = try({
                r = httr::GET(query)
                d = httr::content(r, as='text', encoding='UTF-8')
                d = read_csv(d, col_types = cols(.default = 'c'), skip = 2) %>%
                    mutate(County = clean_county_names(County)) %>%
                    filter(County %in% clean_county_names(cities$county[cities$state == stt]))
            }, silent = TRUE)

            if(inherits(tryresp, 'try-error')){
                dmr_fail = c(dmr_fail, paste(i, stt, yr))
                next
            } else if(nrow(d)){
                dmrd = bind_rows(dmrd, d)
            }
        }
    }

    write_csv(dmrd, paste0('data/dmr/', casrn, '.csv'))
}


## 5 - ECHO (NPDES): data download ####

#SOURCE: https://echo.epa.gov/tools/data-downloads/icis-npdes-data-set
#   click on texas, kentucky, and louisiana. unzip resulting downloads. put them in ./data/echo
#DEFINITIONS: https://echo.epa.gov/tools/data-downloads/icis-npdes-download-summary

#need to run section 5 before this will work

#need this for mapping DMR parameter codes to CAS numbers
d = map_dfr(list.files('data/dmr', full.names = TRUE),
            read_csv, col_types = cols(.default = 'c'))

permit_facility_mapping = distinct(d, `NPDES Permit Number`, FRS_ID=`FRS ID`,
                                   `Facility Latitude`, `Facility Longitude`,
                                   State, County)
paramcode_cas_mapping = distinct(d, `Parameter Code`, `CAS Number`)

dmr_chem_codes = unique(d$`Parameter Code`)

#does not connect violations data to facilities
# lims = read_csv('data/echo/NPDES_LIMITS.csv',
#                 col_types = cols(.default = 'c'))

# echo_facil = read_csv('data/echo/facility_data/FRS_FACILITIES.csv',
#                       col_types = cols(.default = 'c'))
# echo_facil = read_csv('data/echo/facility_data/FRS_NAICS_CODES.csv',
#                       col_types = cols(.default = 'c'))
# echo_facil = read_csv('data/echo/facility_data/FRS_PROGRAM_LINKS.csv',
#                       col_types = cols(.default = 'c'))
# echo_facil = read_csv('data/echo/facility_data/FRS_SIC_CODES.csv',
#                       col_types = cols(.default = 'c'))

#national data for facilities (no chem)
# echo = read_csv('data/echo/ECHO_EXPORTER.csv',
#                 col_types = cols(.default = 'c'))

echo = map_dfr(c('data/echo/LA_NPDES_EFF_VIOLATIONS.csv',
                 'data/echo/TX_NPDES_EFF_VIOLATIONS.csv',
                 'data/echo/KY_NPDES_EFF_VIOLATIONS.csv'),
               read_csv, col_types = cols(.default = 'c'))

echo2 = echo %>%
    filter(PARAMETER_CODE %in% dmr_chem_codes,
           grepl('numeric violation$', VIOLATION_DESC, ignore.case = TRUE))

#still missing some facility data. pick up anything we've missed
unaccounted_permits = echo2 %>%
    filter(! NPDES_ID %in% unique(d$`NPDES Permit Number`)) %>%
    distinct(NPDES_ID) %>%
    pull()

permit_facility_mapping2 = tibble()
for(up in unaccounted_permits){

    query_base = 'https://echodata.epa.gov/echo/dmr_rest_services.get_custom_data_annual?output=CSV'
    query_mid = '&p_est=Y&p_loads_data=DMR&p_nd=ZERO&p_nutrient_agg=N&p_param_group=N'
    permit_addon = paste0('&p_permit_id=', up)

    query = paste0(query_base, query_mid, permit_addon)

    tryresp = try({
        r = httr::GET(query)
        d = httr::content(r, as='text', encoding='UTF-8')
        d = read_csv(d, col_types = cols(.default = 'c'), skip = 2) %>%
            mutate(County = clean_county_names(County))

        print(unique(d$County))

        permit_facility_mapping2 = d %>%
            filter(County %in% clean_county_names(cities$county)) %>%
            slice(1) %>%
            select(`Parameter Code`, `CAS Number`, `NPDES Permit Number`,
                   FRS_ID =`FRS ID`, `Facility Latitude`, `Facility Longitude`,
                   State, County) %>%
            bind_rows(permit_facility_mapping2)
    })
}

permit_facility_mapping = bind_rows(permit_facility_mapping,
                                    select(permit_facility_mapping2,
                                           -any_of(c('CAS Number', 'Parameter Code'))))
paramcode_cas_mapping = bind_rows(paramcode_cas_mapping,
                                    select(permit_facility_mapping2,
                                           `CAS Number`, `Parameter Code`))

echo = echo2 %>%
    left_join(permit_facility_mapping,
              by = c(NPDES_ID = 'NPDES Permit Number')) %>%
    filter(! is.na(County)) %>%
    left_join(paramcode_cas_mapping,
              by = c(PARAMETER_CODE = 'Parameter Code'))

write_csv(echo, 'data/echo/npdes_violations.csv')

## x - other things to attempt? ####

#combined air emissions here: https://echo.epa.gov/tools/data-downloads#downloads

#compile data from ECHO? (or already covered by the above?

#compile more event details using LIMIT_ID, DMR_EVENT_ID, DMR_FORM_ID
