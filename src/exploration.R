library(tidyverse)
library(httr)
library(glue)
library(feather)
library(mapview)
library(lubridate)

mv = mapview::mapview
sw = suppressWarnings
sm = suppressMessages

#TODO:
#find a way to get variant spellings of county names in TRI. could just search for all alternatives:
#   ST JOHN, ST. JOHN, SAINT JOHN, SAINT JOHN THE BAPTIST

setwd('earthjustice/chem_risk_eval/')

## 0 - setup ####

source('src/globals.R')

## 1 [RUN ONCE] - retrieve (and write) more information on each substance

# d = get_substance_by_cas(cas$CASRN_nohyphens)
#
# cas2 = d %>%
#     select(-any_of(c('classifications', 'characterics', 'synonyms', 'casNumbers',
#                      'relationships', 'taxonomicSerialNumbers'))) %>%
#     right_join(cas, by = c(currentCasNumber='CASRN')) %>%
#     select(ej_name=name, CASRN=currentCasNumber, CASRN_nohyphens, SRS_id=internalTrackingNumber,
#            everything()) %>%
#     arrange(ej_name)
#
# write_csv(cas2, 'data/general/target_substances.csv')

cas = read_csv('data/general/target_substances.csv', col_types=cols())

cities = read_csv('data/general/cities.csv', col_types=cols())

## 2 - [ABANDONED] get everything we can out of envirofacts using REST service (ICIS-AIR centric) ####

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

## 3 - TRI: envirofacts REST service ####

# for(i in seq_len(nrow(cas))){
for(i in 4:29){

    srs_id = cas$SRS_id[i]
    print(paste('working on chem:', srs_id))

    #chemicals
    chems = query_ef_table(table_name=c('TRI_CHEM_INFO'), column_name='SRS_ID', operator='=',
                           column_value=as.character(srs_id), warn=TRUE)
    if(nrow(chems) > 1) stop('hm? pick a row.')

    #facilities
    facil_fails = c()
    facil = tibble()
    for(j in seq_len(nrow(cities))){

        facil_ = tibble()
        city = cities[j, ]
        cnty_srch = is.na(city$city)
        colq = if(cnty_srch) 'COUNTY_NAME' else 'CITY_NAME'
        colv = if(cnty_srch) city$county else city$city

        tryres = try({
            facil_ = query_ef_table(table_name=c('TRI_FACILITY'), column_name=colq,
                                    operator='=', column_value=colv, warn=TRUE)

            if(nrow(facil_)) {
                if(! cnty_srch){
                    facil_ = filter(facil_, COUNTY_NAME == city$county & STATE_ABBR == city$state)
                } else {
                    facil_ = filter(facil_, STATE_ABBR == city$state)
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

    #report forms
    rep_frms = query_ef_table(table_name=c('TRI_REPORTING_FORM'), column_name='TRI_CHEM_ID', operator='=',
                              column_value=chems$TRI_CHEM_ID, warn=TRUE)
    rep_frms = filter(rep_frms, TRI_FACILITY_ID %in% uniq_facils)
    docnums = unique(rep_frms$DOC_CTRL_NUM)

    #releases
    release_fails = c()
    releases = tibble()
    for(docnum in docnums){
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

    #environments
    water_fails = c()
    water = tibble()
    for(docnum in docnums){
        water_ = tibble()
        tryres = try({
            water_ = query_ef_table(table_name=c('TRI_WATER_STREAM'),
                                      column_name='DOC_CTRL_NUM', operator='=',
                                      column_value=docnum, warn=TRUE)
            water = bind_rows(water, water_)
        })

        if(inherits(tryres, 'try-error')){
            water_fails = c(water_fails, docnum)
            next
        }
    }

    # system('spd-say chicken')
    # dd = releases[duplicated(releases$DOC_CTRL_NUM), ]
    # dd[duplicated(dd$DOC_CTRL_NUM), ]
    # intersect(colnames(rep_frms), colnames(facil))
    # intersect(colnames(rep_frms), colnames(chems))

    #write data
    chemd = full_join(rep_frms, facil, by = 'TRI_FACILITY_ID') %>%
        full_join(chems, by = 'TRI_CHEM_ID') %>%
        full_join(releases, by = 'DOC_CTRL_NUM') %>%
        full_join(water, by = 'DOC_CTRL_NUM')

    write_csv(chemd, paste0('data/tri/raw/', cas$CASRN[i], '.csv'))

    #write map
    site_locs = chemd %>%
        select(lon=FAC_LONGITUDE, lat=FAC_LATITUDE, everything()) %>%
        # distinct() %>%
        filter(! is.na(lon) & ! is.na(lat)) %>%
        mutate(lon = -sw(dms_to_decdeg(lon)),
               lat = sw(dms_to_decdeg(lat))) %>%
        filter(! is.na(lon) & ! is.na(lat)) %>%
        sf::st_as_sf(coords=c('lon', 'lat'),
                     crs = 4326)

    sites_all = mv(site_locs)
    sites_with_releases = filter(site_locs, ! is.na(TOTAL_RELEASE))

    if(nrow(sites_with_releases)){
        sites_with_releases = mv(sites_with_releases, color = 'red')
    } else {
        sites_with_releases = NULL
    }

    mapshot(sites_all + sites_with_releases,
            url = paste0('figs/tri_', cas$CASRN[i], '.html'))
}


## 4 - [ZERO DATA] NEI: envirofacts REST service ####

#no information on any of our target chemicals!

for(i in seq_len(nrow(cas))){

    srs_id = cas$SRS_id[i]
    print(paste('working on chem:', srs_id))

    #chemicals
    pollutants = query_ef_table(table_name=c('REF_POLLUTANT'), column_name='SRS_ID',
                                operator='=', column_value=as.character(srs_id), warn=FALSE)

    #use this to demonstrate lack of data
#     for(k in seq_len(nrow(pollutants))){
#         print(paste(k, 'of', nrow(pollutants), 'k'))
#         # rr = query_ef_rows(table_name='FACILITY_SUMMARY',
#         rr = query_ef_rows(table_name='POLLUTANT',
#                       column_name=c('POLLUTANT_CODE'),
#                       operator=c('='),
#                       # column_value='108883')
#                       column_value=pollutants$POLLUTANT_CODE[k])
#         print(rr)
#     }
# }

    #facilities (ready to go, just nothing to work with here)
    facil_fails = c()
    facil = tibble()
    for(j in seq_len(nrow(cities))){

        city = cities[j, ]

        for(k in seq_len(nrow(pollutants))){

            facil_ = tibble()

            tryres = try({
                facil_ = query_ef_table(table_name='FACILITY_SUMMARY',
                                        column_name=c('STATE', 'COUNTY', 'POLLUTANT_CODE'),
                                        operator=c('=', '=', '='),
                                        column_value=c(city$state,
                                                       str_to_sentence(city$county),
                                                       pollutants$POLLUTANT_CODE[k]),
                                        warn=FALSE)

                if(nrow(facil_)){
                    print(paste('got', nrow(facil_), 'rows for', city$county, pollutants$CHEMICAL_ABSTRACT_SERVICE_NMBR[k]))
                    facil_ = filter(facil_, STATE == city$state)
                    facil = bind_rows(facil, facil_)
                }
            })

            if(inherits(tryres, 'try-error')){
                facil_fails = c(facil_fails, j)
                next
            }
        }
    }
}


## 5 - [no ECHO service per se] ECHO: envirofacts REST service ####
## 6 - DMR: REST service ####

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
            })

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


## x - other things to attempt? ####

#compile data from ECHO? (or already covered by the above?

#compile location info and bind to event/chem info

#compile more event details using LIMIT_ID, DMR_EVENT_ID, DMR_FORM_ID

## 3 - EPA's TRI-DRM comparison portal ####
#(water pollutant loading tool custom search)
#https://echo.epa.gov/trends/loading-tool/get-data/custom-search/

d = read_csv('data/chem/epa_dmr_tri_portal/louisville_2020.csv',
             skip=3,
             name_repair='universal')

d_tab = table(d$CAS.Number)

cas$CASRN_nohyphens %in% names(d_tab)

dd = d %>%
    filter(CAS.Number == 106467)
