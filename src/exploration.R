library(tidyverse)

cas = read_lines('data/general/cas_numbers.txt')
cas = tibble(hyphens = cas,
             nohyphens = as.numeric(gsub('-', '', cas)))

## EPA's TRI-DRM comparison portal
#(water pollutant loading tool custom search)
#https://echo.epa.gov/trends/loading-tool/get-data/custom-search/

d = read_csv('data/chem/epa_dmr_tri_portal/louisville_2020.csv',
             skip = 3,
             name_repair = 'universal')

d_tab = table(d$CAS.Number)

cas$nohyphens %in% names(d_tab)

dd = d %>%
    filter(CAS.Number == 106467)


##

d = read_csv('data/chem/ices_air/louisville.csv',
             name_repair = 'universal')

d$
