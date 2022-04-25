library(terra)
library(raster)
library(tidyverse)
library(glue)
library(lubridate)
# library(KernSmooth)
library(leaflet)
library(mapview)
library(viridis)
library(ggplot2)
library(scales)
# library(magick)
# library(htmlwidgets)
# library(htmltools)

sw = suppressWarnings
sm = suppressMessages

# setup ####

source('src/00_globals.R')

cas = read_csv('data/general/target_substances.csv', col_types=cols())

cities = read_csv('data/general/cities.csv', col_types=cols())

houston_counties = cities %>%
    filter(! is.na(city) & city == 'HOUSTON') %>%
    pull(county) %>%
    clean_county_names()

emissions = read_csv('data/emissions_harmonized_2010-22.csv',
                     col_types = 'iccicnnnlclc')

# maps ####

for(loc in unique(emissions$target_location)){

    dd = filter(emissions, target_location == loc)

    if(loc == 'Houston'){
        map_scale = 10
    } else if(loc == 'Port Arthur'){
        map_scale = 10
    } else if(loc == 'Louisville'){
        map_scale = 10
    } else {
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

# boxplots ####

emissions %>%
    group_by(cas, source) %>%
    summarize(load_kg = sum(load_kg)) %>%
    ungroup() %>%
    print(n=100)

## stacked bars for combined emissions

emissions %>%
    rename(Source = source) %>%
    # filter(county %in% houston_counties) %>%
    group_by(year, Source, target_location) %>%
    summarize(load_kg = sum(load_kg)) %>%
    ungroup() %>%
    ggplot(aes(fill = Source, x = year, y = load_kg)) +
    geom_bar(position = 'stack', stat = 'identity') +
    # scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6, big.mark = ',')) +
    scale_y_continuous(labels = label_number(suffix = ' K', scale = 1e-3, big.mark = '')) +
    scale_x_continuous(name = NULL, breaks = 2010:2022, labels = 2010:2022) +
    # scale_y_continuous(label = math_format()) +
    # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
    #               labels = trans_format("log10", math_format(10^.x))) +
    # annotation_logticks()
    facet_wrap(.~target_location, scales = 'free_y') +
    labs(title = 'Combined emissions across 14 of 24 high-priority chemicals, by source and target location',
         y = 'Emissions (kg)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw()
    # scale_fill_viridis_d()

## stacked bars for RSEI weights

TODO

# emissions %>%
#     rename(Source = source) %>%
#     # filter(county %in% houston_counties) %>%
#     group_by(year, Source, target_location) %>%
#     summarize(load_kg = sum(load_kg)) %>%
#     ungroup() %>%
#     ggplot(aes(fill = Source, x = year, y = load_kg)) +
#     geom_bar(position = 'stack', stat = 'identity') +
#     # scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6, big.mark = ',')) +
#     scale_y_continuous(labels = label_number(suffix = ' K', scale = 1e-3, big.mark = '')) +
#     scale_x_continuous(name = NULL, breaks = 2010:2022, labels = 2010:2022) +
#     # scale_y_continuous(label = math_format()) +
#     # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#     #               labels = trans_format("log10", math_format(10^.x))) +
#     # annotation_logticks()
#     facet_wrap(.~target_location, scales = 'free_y') +
#     labs(title = 'Combined emissions across 14 of 24 high-priority chemicals, by source and target location',
#          y = 'Emissions (kg)') +
#     guides(x = guide_axis(angle = 45)) +
#     theme_bw()
#     # scale_fill_viridis_d()
#
# ggsave('figs/plots/stacked_bars_emissions.png', width = 8, height = 8)

## bars for NPDES

emissions %>%
    filter(source == 'NPDES') %>%
    mutate(cas = as.character(cas)) %>%
    rename(CASRN = cas) %>%
    group_by(CASRN, year, target_location) %>%
    summarize(load_kg = sum(load_kg)) %>%
    ungroup() %>%
    ggplot(aes(fill = CASRN, x = year, y = load_kg)) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_x_continuous(name = NULL, breaks = 2010:2022, labels = 2010:2022) +
    facet_wrap(.~target_location, scales = 'free') +
    labs(title = 'NPDES illegal emissions by CAS Registry Number and target location',
         y = 'Emissions (kg)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw()

ggsave('figs/plots/illegal_emissions.png', width = 8, height = 4)

## bars for DMR

emissions %>%
    filter(source == 'DMR') %>%
    mutate(cas = as.character(cas)) %>%
    rename(CASRN = cas) %>%
    group_by(CASRN, year, target_location) %>%
    summarize(load_kg = sum(load_kg)) %>%
    ungroup() %>%
    ggplot(aes(fill = CASRN, x = year, y = load_kg)) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_x_continuous(name = NULL, breaks = 2010:2022, labels = 2010:2022) +
    facet_wrap(.~target_location, scales = 'free_y') +
    labs(title = 'DMR emissions by CAS Registry Number and target location',
         y = 'Emissions (kg)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw()

ggsave('figs/plots/DMR_emissions.png', width = 8, height = 8)

## bars for TRI

emissions %>%
    filter(source == 'TRI') %>%
    mutate(cas = as.character(cas)) %>%
    rename(CASRN = cas) %>%
    group_by(CASRN, year, target_location) %>%
    summarize(load_kg = sum(load_kg)) %>%
    ungroup() %>%
    ggplot(aes(fill = CASRN, x = year, y = load_kg)) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_x_continuous(name = NULL, breaks = 2010:2022, labels = 2010:2022) +
    facet_wrap(.~target_location, scales = 'free_y') +
    labs(title = 'TRI emissions by CAS Registry Number and target location',
         y = 'Emissions (kg)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw()

ggsave('figs/plots/TRI_emissions.png', width = 8, height = 8)

## bars for NEI

emissions %>%
    filter(source == 'NEI') %>%
    mutate(cas = as.character(cas)) %>%
    rename(CASRN = cas) %>%
    group_by(CASRN, year, target_location) %>%
    summarize(load_kg = sum(load_kg)) %>%
    ungroup() %>%
    ggplot(aes(fill = CASRN, x = year, y = load_kg)) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_x_continuous(name = NULL, breaks = 2010:2022, labels = 2010:2022) +
    facet_wrap(.~target_location, scales = 'free_y') +
    labs(title = 'NEI emissions by CAS Registry Number and target location',
         y = 'Emissions (kg)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw()

ggsave('figs/plots/NEI_emissions.png', width = 8, height = 8)

