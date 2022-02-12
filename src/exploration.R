library(tidyverse)
library(httr)
library(glue)


## 0 - setup ####

source('src/globals.R')

cas = read_csv('data/general/target_substances.csv')

##  1 [RUN ONCE] - retrieve (and write) more information on each substance ####

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


## 2 - get everything we can out of envirofacts using REST service ####
#ICIS-AIR, ECHO?, etc

query_envirofacts(table_names,
                  column_name=NULL,
                  operator=NULL,
                  column_value=NULL,
                  rows=NULL,
                  # output_format='CSV',
                  return_count=FALSE)


## 3 -EPA's TRI-DRM comparison portal
#(water pollutant loading tool custom search)
#https://echo.epa.gov/trends/loading-tool/get-data/custom-search/

d = read_csv('data/chem/epa_dmr_tri_portal/louisville_2020.csv',
             skip=3,
             name_repair='universal')

d_tab = table(d$CAS.Number)

cas$CASRN_nohyphens %in% names(d_tab)

dd = d %>%
    filter(CAS.Number == 106467)
