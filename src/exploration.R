library(tidyverse)
library(httr)
library(glue)

# setwd('earthjustice/chem_risk_eval/')

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

## 2 - get everything we can out of envirofacts using REST service ####

# compile data from ICIS-AIR
icis_air_chem = tibble()
for(i in seq_len(nrow(cas))){

    srs_id = cas$SRS_id[i]

    icis_air_chem_ = query_envirofacts(
        table_names=c('ICIS_DMR_FORM_PARAMETER', 'ICIS_ENF_CONCL_POLLUTANT',
                      'ICIS_LIMIT', 'REF_POLLUTANT', 'TRI_CHEM_INFO'),
                      # icis_air_chem = query_envirofacts(table_names=c('ICIS_LIMIT'),
        filter_column='SRS_ID',
        filter_operator='=',
        filter_value=srs_id,
        join_column='SRS_ID',
        save_intermediates=TRUE,
        warn=FALSE)

    #currently exploring in get_ef_tablechunk. see HERE note there
    aaa = query_ef_table(table_name=c('ICIS_LIMIT'), column_name='SRS_ID', operator='=', column_value=as.character(srs_id), warn=TRUE)
    aaa = query_ef_table(table_name=c('TRI_CHEM_INFO'), column_name='SRS_ID', operator='=', column_value=as.character(srs_id), warn=TRUE)
    aaa = query_ef_table(table_name=c('ICIS_ENF_CONCL_POLLUTANT'), column_name='SRS_ID', operator='=', column_value=as.character(srs_id), warn=TRUE)
    aaa = query_ef_table(table_name=c('REF_POLLUTANT'), column_name='SRS_ID', operator='=', column_value=as.character(srs_id), warn=TRUE)
    aaa = query_ef_table(table_name=c('ICIS_DMR_FORM_PARAMETER'), column_name='SRS_ID', operator='=', column_value=as.character(srs_id), warn=TRUE)

    #save temporary output in case of error, power failure, etc.
    saveRDS(icis_air_chem_, paste0('~/temp/icis_air_', srs_id, '.rds'))
    # assign(paste0('icis_air_', srs_id), icis_air_chem_, pos=.GlobalEnv)

    icis_air_chem = bind_rows(icis_air_chem, icis_air_chem_)
}

#compile data from ECHO?


## 3 -EPA's TRI-DRM comparison portal ####
#(water pollutant loading tool custom search)
#https://echo.epa.gov/trends/loading-tool/get-data/custom-search/

d = read_csv('data/chem/epa_dmr_tri_portal/louisville_2020.csv',
             skip=3,
             name_repair='universal')

d_tab = table(d$CAS.Number)

cas$CASRN_nohyphens %in% names(d_tab)

dd = d %>%
    filter(CAS.Number == 106467)
