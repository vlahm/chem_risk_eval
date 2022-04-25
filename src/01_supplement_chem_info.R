library(tidyverse)
library(httr)
library(glue)

setwd('~/git/earthjustice/chem_risk_eval/')

## 0 - setup ####

source('src/00_globals.R')

tox = readxl::read_xlsx('data/general/toxicity_data_rsei_v239.xlsx', sheet = 2,
                        col_types = 'text')

#this CSV needs a column called "CASRN", containing CAS numbers with hyphens,
#and a column called "ej_name", containing chemical names as recognized by EarthJustice
cas = read_csv('data/general/target_substances.csv', col_types=cols(.default = 'c'))

if(! all(c('ej_name', 'CASRN') %in% colnames(cas))){
    stop('data/general/target_substances.csv must have columns "ej_name" and "CASRN"')
}

if('molecularFormula' %in% colnames(cas)){
    stop('looks like this script has already been run. you only need to run it once.')
}

## 1 - retrieve more information on each substance ####

cas$CASRN_nohyphens = gsub('-', '', cas$CASRN)
d = get_substance_by_cas(cas$CASRN_nohyphens)

cas = d %>%
    select(-any_of(c('classifications', 'characterics', 'synonyms', 'casNumbers',
                     'relationships', 'taxonomicSerialNumbers'))) %>%
    right_join(cas, by = c(currentCasNumber='CASRN')) %>%
    select(ej_name, CASRN=currentCasNumber, CASRN_nohyphens, SRS_id=internalTrackingNumber,
           everything()) %>%
    select(-any_of(c('characteristics', 'pubChemId', 'epaIdentificationNumber',
                     'currentTaxonomicSerialNumber', 'kingdomCode'))) %>%
    arrange(ej_name)

## 2 - join RSEI toxicity weights and write back ####

cas %>%
    left_join(select(tox, CASRN = CASStandard, rsei_weight = MaxTW)) %>%
    write_csv('data/general/target_substances.csv')
