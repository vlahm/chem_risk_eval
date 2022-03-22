library(tidyverse)
library(httr)
library(glue)
library(feather)

#TODO:
# [!] allow multiple column filtering operations in query_ef_table
# [?] add parameter multi_index_action. can choose to drop all but the join index, join on all columns ending in "_ID",
# [?] join on all shared columns, interactively drop shared columns, etc.


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

## 2 - get everything we can out of envirofacts using REST service (ICIS-AIR centric) ####

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

## 3 - get everything we can out of envirofacts using REST service (TRI centric) ####

HERE: trimmed, but unmodified from ICIS-AIR approach
for(i in seq_len(nrow(cas))){

    srs_id = cas$SRS_id[i]

    # redundant_chem_cols_1 = c('POLLUTANT_DESC', 'CHEMICAL_FORMULA', 'CHEMICAL_ABSTRACT_SERVICE_NMBR',
    #                           'EPA_ID', 'SRS_SYSTEMATIC_NAME')
    # redundant_chem_cols_2 = c('MONITORING_LOCATION_DESC',
    #                           'PARAMETER_DESC', 'PERCENT_EXCEEDENCE_FLAG', 'PERCENT_REMOVAL_FLAG',
    #                           'POLLUTANT_CATEGORY_DESC')

    chems = query_ef_table(table_name=c('TRI_CHEM_INFO'), column_name='SRS_ID', operator='=',
                           column_value=as.character(srs_id), warn=TRUE)
    if(nrow(chems) > 1) stop('hm? pick a row.')

    facil = query_ef_table(table_name=c('TRI_FACILITY'), column_name=c('CITY_NAME'), operator='=',
                           column_value=c('LOUISVILLE'), warn=TRUE)
    facil = filter(facil,
                   COUNTY_NAME == 'JEFFERSON',
                   STATE_ABBR == 'KY')

    rep_frms = query_ef_table(table_name=c('TRI_REPORTING_FORM'), column_name='TRI_CHEM_ID', operator='=',
                              column_value=chems$TRI_CHEM_ID, warn=TRUE)

    releases = query_ef_table(table_name=c('TRI_RELEASE_QTY'), column_name='DOC_CTRL_NUM', operator='=',
                              column_value=chems$TRI_CHEM_ID, warn=TRUE)

    chemd = full_join(rep_frms, facil, by = 'TRI_FACILITY_ID') %>%
        full_join(chems, by = 'TRI_CHEM_ID')

    intersect(colnames(rep_frms), colnames(facil))
    intersect(colnames(rep_frms), colnames(chems))


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

    #     r1 = query_ef_rows(table_name=c('TRI_CHEM_INFO'), column_name='POLLUTANT_CODE', operator='=', column_value=as.character(pollutants$POLLUTANT_CODE_pol))
    #     r2 = query_ef_rows(table_name=c('ICIS_ENF_CONCL_POLLUTANT'), column_name='SRS_ID', operator='=', column_value=as.character(srs_id))
    #     print(paste(r1, r2))
    # }

    # tri_chem = query_ef_table(table_name=c('TRI_CHEM_INFO'), column_name='POLLUTANT_CODE', operator='=', column_value=as.character(pollutants$POLLUTANT_CODE_pol), warn=TRUE)
    # dmr_forms = query_ef_table(table_name=c('ICIS_DMR_FORM_PARAMETER'), column_name='SRS_ID', operator='=', column_value=as.character(srs_id), warn=TRUE) %>%
    #     select(-any_of(c(redundant_chem_cols_1, redundant_chem_cols_2))) %>%
    #     rename_with(~paste0(., '_dmr'), -ends_with('_ID'))

    chemd = dmr_forms %>%
        left_join(limits,
                  by = c('SRS_ID', 'LIMIT_ID')) %>%
        left_join(pollutants,
                  by = c('SRS_ID')) %>%
        select(ends_with('_ID'), everything())

    write_feather(chemd, paste0('data/envirofacts_chemtables/icis_air/', srs_id, '.feather'))

    # for(j in unique(chemd$){
    #
    #
    # }
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
