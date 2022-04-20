library(terra)
library(raster)
library(tidyverse)
library(glue)
library(lubridate)
# library(KernSmooth)
library(leaflet)
library(mapview)
library(viridis)
library(magick)
# library(htmlwidgets)
# library(htmltools)

sw = suppressWarnings
sm = suppressMessages

# setup ####

source('src/globals.R')

cas = read_csv('data/general/target_substances.csv', col_types=cols())

cities = read_csv('data/general/cities.csv', col_types=cols())

houston_counties = cities %>%
    filter(! is.na(city) & city == 'HOUSTON') %>%
    pull(county) %>%
    clean_county_names()

emissions = read_csv('data/emissions_harmonized.csv')

# maps ####

for(loc in c('Houston', 'Port Arthur', 'Louisville', 'Cancer Alley')){

    if(loc == 'Houston'){
        dd = filter(emissions, state == 'TX', county %in% houston_counties)
        map_scale = 10
    } else if(loc == 'Port Arthur'){
        dd = filter(emissions, state == 'TX', county == 'JEFFERSON')
        map_scale = 10
    } else if(loc == 'Louisville'){
        dd = filter(emissions, state == 'KY')
        map_scale = 10
    } else {
        dd = filter(emissions, state == 'LA')
        map_scale = 9
    }

    map_center = c(mean(dd$lon, na.rm=TRUE),
                   mean(dd$lat, na.rm=TRUE))

    latrange = range(dd$lat, na.rm = TRUE)
    latrange[1] = latrange[1] - 0.1
    latrange[2] = latrange[2] + 0.1
    lonrange = range(dd$lon, na.rm = TRUE) + 0.1
    lonrange[1] = lonrange[1] - 0.1
    lonrange[2] = lonrange[2] + 0.1

    if(nrow(dd)){
        ej_heatmap(dd, center = map_center, scale = map_scale, res = 1/60,
                   latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                   fileout = glue('figs/heatmaps/{lc}_allchems_allyears.html',
                                  lc = gsub(' ', '_', tolower(loc))))
    } else next

    for(chem in cas$CASRN_nohyphens){

        ddc = filter(dd, cas == chem)

        if(nrow(ddc)){
            ej_heatmap(ddc, center = map_center, scale = map_scale, res = 1/60,
                       latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                       fileout = glue('figs/heatmaps/by_chem/{lc}_{ch}_allyears.html',
                                      lc = gsub(' ', '_', tolower(loc)),
                                      ch = chem))
        } else next

        for(yr in 2010:2021){

            ddy = filter(ddc, year == yr)

            if(nrow(ddy)){
                ej_heatmap(ddy, center = map_center, scale = map_scale, res = 1/60,
                           latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                           fileout = glue('figs/heatmaps/by_chem/by_year/{lc}_{ch}_{yr}.html',
                                          lc = gsub(' ', '_', tolower(loc)),
                                          ch = chem,
                                          yr = yr))
            }
        }

        #HERE: USE THE COMMENTED CODE BELOW TO STITCH PDFS FOR EACH CHEM BY LOCATION
        #THEN BIND BY YEAR INTO A GIF
    }

    #AND HERE, DO THE SAME ACROSS ALL CHEMS?
}

# list.files('figs/heatmaps/by_chem/by_year/...', pattern = '*.png', full.names = TRUE) %>%
#     image_read() %>% # reads each path file
#     image_join() %>% # joins image
#     image_animate(fps=4) %>% # animates, can opt for number of loops
#     image_write("FileName.gif") # write to current dir
