library(tidyverse)

# source('src/00_globals.R')
cas = read_csv('data/general/target_substances.csv', col_types=cols())

d = read_csv('data/emissions_corrected_TRIpriority_2010-22.csv')

worst_facilities_hou = d %>%
    filter(target_location == 'Houston') %>%
    group_by(frs_id) %>%
    summarize(load_lb = sum(load_lb)) %>%
    ungroup() %>%
    arrange(desc(load_lb))

worst_facilities_art = d %>%
    filter(target_location == 'Port Arthur') %>%
    group_by(frs_id) %>%
    summarize(load_lb = sum(load_lb)) %>%
    ungroup() %>%
    arrange(desc(load_lb))

facil_d = function(frs_id, filen){

    dd = d %>%
        filter(frs_id == !!frs_id) %>%
        left_join(select(cas, cas = CASRN_nohyphens, ej_name), by = 'cas') %>%
        select(year, ej_name, medium, load_lb, source) %>%
        arrange(ej_name, year, medium, source)

    write_csv(dd, filen)
}

#houston
# facil_d(110000463221, '/tmp/dingo.csv')
# facil_d(110000606933, '/tmp/dingo.csv')
# facil_d(110000463212, '/tmp/dingo.csv')
facil_d(110031267064, '/tmp/dingo.csv')#1
# facil_d(110000503722, '/tmp/dingo.csv')
facil_d(110000463169, '/tmp/dingo.csv')#2
# facil_d(110038101023, '/tmp/dingo.csv')
# facil_d(110008170237, '/tmp/dingo.csv')
facil_d(110000502901, '/tmp/dingo.csv')#3
facil_d(110000461134, '/tmp/dingo.csv')#4
#---
facil_d(110032992215, '/tmp/dingo.csv')#5
facil_d(110017769734, '/tmp/dingo.csv')#6
facil_d(110000462231, '/tmp/dingo.csv')#7
facil_d(110070827819, '/tmp/dingo.csv')#8
facil_d(110012511120, '/tmp/dingo.csv')#9
facil_d(110017743281, '/tmp/dingo.csv')#10

#port arthur
facil_d(110000755200, '/tmp/dingo.csv')#8
facil_d(110000504801, '/tmp/dingo.csv')#7
# facil_d(110041419257, '/tmp/dingo.csv')
facil_d(110006134691, '/tmp/dingo.csv') #1
# facil_d(110062118505, '/tmp/dingo.csv')
# facil_d(110041044063, '/tmp/dingo.csv')
# facil_d(110041126484, '/tmp/dingo.csv')
# facil_d(110000464131, '/tmp/dingo.csv')
# facil_d(110041049638, '/tmp/dingo.csv')
# facil_d(110041990913, '/tmp/dingo.csv')
#---
facil_d(110000599567, '/tmp/dingo.csv')
facil_d(110020515735, '/tmp/dingo.csv')
facil_d(110000861862, '/tmp/dingo.csv')
facil_d(110000464024, '/tmp/dingo.csv')
facil_d(110000464006, '/tmp/dingo.csv')
