# library(terra) #only needed if plotting heatmaps
# library(raster) #only needed if plotting heatmaps
library(sf)
library(tidyverse)
library(glue)
library(lubridate)
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

emissions = read_csv('data/emissions_harmonized_excess_distributed_evenly_2010-22.csv',
                     col_types = 'iccicnnnclc')

sources = unique(emissions$source)

#create directory structure for storing plots and maps
dir.create('figs/plots/', recursive = TRUE, showWarnings = FALSE)
dir.create('figs/load_maps/by_source_and_location', recursive = TRUE, showWarnings = FALSE)
dir.create('figs/load_maps/by_source_and_location/by_chem', showWarnings = FALSE)
dir.create('figs/load_maps/by_source_and_location/by_year', showWarnings = FALSE)
dir.create('figs/load_maps/by_source_and_location/by_chem/by_year', showWarnings = FALSE)
dir.create('figs/rsei_maps/by_source_and_location', recursive = TRUE, showWarnings = FALSE)
dir.create('figs/rsei_maps/by_source_and_location/by_chem', showWarnings = FALSE)
dir.create('figs/rsei_maps/by_source_and_location/by_year', showWarnings = FALSE)
dir.create('figs/rsei_maps/by_source_and_location/by_chem/by_year', showWarnings = FALSE)
dir.create('figs/shapefiles/by_source_and_location', recursive = TRUE, showWarnings = FALSE)
dir.create('figs/shapefiles/by_source_and_location/by_chem', showWarnings = FALSE)
dir.create('figs/shapefiles/by_source_and_location/by_year', showWarnings = FALSE)
dir.create('figs/shapefiles/by_source_and_location/by_chem/by_year', showWarnings = FALSE)

# emissions and RSEI maps, and shapefile output ####

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

        for(src in sources){

            ddfilt = filter(dd, source == !!src)

            ddo = ddfilt %>%
                group_by(lat, lon) %>%
                summarize(load_kg = sum(load_kg, na.rm = TRUE),
                          .groups = 'drop')

            ej_map2_pointsize(
                ddo, center = map_center, scale = map_scale, res = 1/60,
                latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                title = glue('{sr}, {lc}: Combined emissions across {nc} of 24 high-priority chemicals, 2010-22',
                             sr = src, nc = length(unique(ddfilt$cas)), lc = loc),
                fileout = glue('figs/load_maps/by_source_and_location/{sr}_{lc}_allchems_allyears.html',
                               sr = tolower(src),
                               lc = gsub(' ', '_', tolower(loc))))

            ddo_rsei = ddfilt %>%
                left_join(select(cas, cas = CASRN_nohyphens, rsei_weight)) %>%
                filter(! is.na(rsei_weight)) %>%
                mutate(rsei_val = load_kg * rsei_weight) %>%
                group_by(lat, lon) %>%
                summarize(load_kg = sum(load_kg, na.rm = TRUE),
                          cas = first(cas),
                          .groups = 'drop')

            if(nrow(ddo_rsei)){
                ej_map2_pointsize(
                    ddo_rsei, center = map_center, scale = map_scale, res = 1/60,
                    latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                    title = glue('{sr}, {lc}: RSEI-weighted emissions across {nc} of 24 high-priority chemicals, 2010-22',
                                 sr = src, nc = length(unique(ddo_rsei$cas)), lc = loc),
                    fileout = glue('figs/rsei_maps/by_source_and_location/{sr}_{lc}_allchems_allyears.html',
                                   sr = tolower(src),
                                   lc = gsub(' ', '_', tolower(loc))),
                    type = 'rsei')
            }

            if(nrow(ddo)){
                ddo %>%
                    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
                    st_write(dsn = 'figs/shapefiles/by_source_and_location/',
                             layer = glue('{sr}_{lc}_allchems_allyears.shp',
                                          sr = tolower(src),
                                          lc = gsub(' ', '_', tolower(loc))),
                             driver = 'ESRI Shapefile',
                             delete_layer = TRUE, quiet = TRUE)
            }

            for(k in 1:nrow(cas)){

                chem = cas$ej_name[k]
                chem_cas = cas$CASRN[k]
                chem_cas_nohyph = cas$CASRN_nohyphens[k]

                ddfilt2 = filter(ddfilt, cas == chem_cas_nohyph)

                ddc = ddfilt2 %>%
                    group_by(lat, lon) %>%
                    summarize(load_kg = sum(load_kg, na.rm = TRUE),
                              .groups = 'drop')

                if(nrow(ddc)){

                    ej_map2_pointsize(
                        ddc, center = map_center, scale = map_scale, res = 1/60,
                        latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                        title = glue('{sr}, {lc}: {ch} (CASRN={cs}) emissions, 2010-22',
                                     sr = src, ch = chem, cs = chem_cas, lc = loc),
                        fileout = glue('figs/load_maps/by_source_and_location/by_chem/{sr}_{lc}_{ch}_allyears.html',
                                       sr = tolower(src),
                                       lc = gsub(' ', '_', tolower(loc)),
                                       ch = chem_cas))

                    ddc_rsei = ddfilt2 %>%
                        left_join(select(cas, cas = CASRN_nohyphens, rsei_weight)) %>%
                        filter(! is.na(rsei_weight)) %>%
                        mutate(rsei_val = load_kg * rsei_weight) %>%
                        group_by(lat, lon) %>%
                        summarize(load_kg = sum(load_kg, na.rm = TRUE),
                                  cas = first(cas),
                                  .groups = 'drop')

                    if(nrow(ddc_rsei)){
                        ej_map2_pointsize(
                            ddc_rsei, center = map_center, scale = map_scale, res = 1/60,
                            latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                            title = glue('{sr}, {lc}: RSEI-weighted {ch} (CASRN={cs}) emissions, 2010-22',
                                         sr = src, ch = chem, cs = chem_cas, lc = loc),
                            fileout = glue('figs/rsei_maps/by_source_and_location/by_chem/{sr}_{lc}_{ch}_allyears.html',
                                           sr = tolower(src),
                                           lc = gsub(' ', '_', tolower(loc)),
                                           ch = chem_cas),
                            type = 'rsei')
                    }

                    ddc %>%
                        st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
                        st_write(dsn = 'figs/shapefiles/by_source_and_location/by_chem/',
                                 layer = glue('{sr}_{lc}_{ch}_allyears.shp',
                                              sr = tolower(src),
                                              lc = gsub(' ', '_', tolower(loc)),
                                              ch = chem_cas),
                                 driver = 'ESRI Shapefile',
                                 delete_layer = TRUE, quiet = TRUE)

                    for(yr in 2010:2022){

                        ddfilt3 = filter(ddfilt2, year == yr)

                        ddy1 = ddfilt3 %>%
                            group_by(lat, lon) %>%
                            summarize(load_kg = sum(load_kg, na.rm = TRUE),
                                      .groups = 'drop')

                        if(nrow(ddy1)){

                            ej_map2_pointsize(
                                ddy1, center = map_center, scale = map_scale, res = 1/60,
                                latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                                title = glue('{sr}, {lc}: {ch} (CASRN={cs}) emissions, {yr}',
                                             sr = src, ch = chem, cs = chem_cas, lc = loc, yr = yr),
                                fileout = glue('figs/load_maps/by_source_and_location/by_chem/by_year/{sr}_{lc}_{ch}_{yr}.html',
                                               sr = tolower(src),
                                               lc = gsub(' ', '_', tolower(loc)),
                                               ch = chem_cas,
                                               yr = yr))

                            ddy1_rsei = ddfilt3 %>%
                                left_join(select(cas, cas = CASRN_nohyphens, rsei_weight)) %>%
                                filter(! is.na(rsei_weight)) %>%
                                mutate(rsei_val = load_kg * rsei_weight) %>%
                                group_by(lat, lon) %>%
                                summarize(load_kg = sum(load_kg, na.rm = TRUE),
                                          cas = first(cas),
                                          .groups = 'drop')

                            if(nrow(ddy1_rsei)){
                                ej_map2_pointsize(
                                    ddy1_rsei, center = map_center, scale = map_scale, res = 1/60,
                                    latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                                    title = glue('{sr}, {lc}: RSEI-weighted {ch} (CASRN={cs}) emissions, {yr}',
                                                 sr = src, ch = chem, cs = chem_cas, lc = loc, yr = yr),
                                    fileout = glue('figs/rsei_maps/by_source_and_location/by_chem/by_year/{sr}_{lc}_{ch}_{yr}.html',
                                                   sr = tolower(src),
                                                   lc = gsub(' ', '_', tolower(loc)),
                                                   ch = chem_cas,
                                                   yr = yr),
                                    type = 'rsei')
                            }

                            ddy1 %>%
                                st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
                                st_write(dsn = 'figs/shapefiles/by_source_and_location/by_chem/by_year/',
                                         layer = glue('{sr}_{lc}_{ch}_{yr}.shp',
                                                      sr = tolower(src),
                                                      lc = gsub(' ', '_', tolower(loc)),
                                                      ch = chem_cas,
                                                      yr = yr),
                                         driver = 'ESRI Shapefile',
                                         delete_layer = TRUE, quiet = TRUE)
                        }
                    }

                } else next
            }

            for(yr in 2010:2022){

                ddfilt4 = filter(ddfilt, year == yr)

                ddy2 = ddfilt4 %>%
                    group_by(lat, lon) %>%
                    summarize(load_kg = sum(load_kg, na.rm = TRUE),
                              .groups = 'drop')

                if(nrow(ddy2)){

                    ej_map2_pointsize(
                        ddy2, center = map_center, scale = map_scale, res = 1/60,
                        latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                        title = glue('{sr}, {lc}: Combined emissions across {nc} of 24 high-priority chemicals, {yr}',
                                     sr = src, nc = length(unique(ddfilt4$cas)), lc = loc, yr = yr),
                        fileout = glue('figs/load_maps/by_source_and_location/by_year/{sr}_{lc}_{yr}_allchems.html',
                                       sr = tolower(src),
                                       lc = gsub(' ', '_', tolower(loc)),
                                       yr = yr))

                    ddy2_rsei = ddfilt4 %>%
                        left_join(select(cas, cas = CASRN_nohyphens, rsei_weight)) %>%
                        filter(! is.na(rsei_weight)) %>%
                        mutate(rsei_val = load_kg * rsei_weight) %>%
                        group_by(lat, lon) %>%
                        summarize(load_kg = sum(load_kg, na.rm = TRUE),
                                  cas = first(cas),
                                  .groups = 'drop')

                    if(nrow(ddy2_rsei)){
                        ej_map2_pointsize(
                            ddy2_rsei, center = map_center, scale = map_scale, res = 1/60,
                            latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                            title = glue('{sr}, {lc}: RSEI-weighted emissions across {nc} of 24 high-priority chemicals, {yr}',
                                         sr = src, nc = length(unique(ddfilt4$cas)), lc = loc, yr = yr),
                            fileout = glue('figs/rsei_maps/by_source_and_location/by_chem/{sr}_{lc}_{yr}_allchems.html',
                                           sr = tolower(src),
                                           lc = gsub(' ', '_', tolower(loc)),
                                           yr = yr),
                            type = 'rsei')
                    }

                    ddy2 %>%
                        st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
                        st_write(dsn = 'figs/shapefiles/by_source_and_location/by_year',
                                 layer = glue('{sr}_{lc}_{yr}_allchems.shp',
                                              sr = tolower(src),
                                              lc = gsub(' ', '_', tolower(loc)),
                                              yr = yr),
                                 driver = 'ESRI Shapefile',
                                 delete_layer = TRUE, quiet = TRUE)
                }
            }

            #HERE: USE THE COMMENTED CODE BELOW TO STITCH PDFS FOR EACH CHEM BY LOCATION
            #THEN BIND BY YEAR INTO A GIF
        }
    }

    #AND HERE, DO THE SAME ACROSS ALL CHEMS?
}

# list.files('figs/load_maps/by_chem/by_year/...', pattern = '*.png', full.names = TRUE) %>%
#     image_read() %>% # reads each path file
#     image_join() %>% # joins image
#     image_animate(fps=4) %>% # animates, can opt for number of loops
#     image_write("FileName.gif") # write to current dir


# list.files('figs/load_maps/by_chem/by_year/...', pattern = '*.png', full.names = TRUE) %>%
#     image_read() %>% # reads each path file
#     image_join() %>% # joins image
#     image_animate(fps=4) %>% # animates, can opt for number of loops
#     image_write("FileName.gif") # write to current dir

# stacked barplots of emissions by location and source ####

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
    labs(title = 'Combined emissions across 14 of 24 high-priority chemicals by year, source, and location',
         y = 'Emissions (kg)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    # scale_fill_viridis_d()

ggsave('figs/plots/combined_emissions.png', width = 8, height = 8)

## bars for NPDES

emissions %>%
    filter(source == 'NPDES') %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
    mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')')) %>%
    group_by(`Chemical (CASRN)`, year, target_location) %>%
    summarize(load_kg = sum(load_kg)) %>%
    ungroup() %>%
    ggplot(aes(fill = `Chemical (CASRN)`, x = year, y = load_kg)) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_x_continuous(name = NULL, breaks = 2010:2022, labels = 2010:2022) +
    facet_wrap(.~target_location, scales = 'free_y') +
    labs(title = 'NPDES illegal emissions by year, chemical, and location',
         y = 'Emissions (kg)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave('figs/plots/illegal_emissions.png', width = 8, height = 4)

## bars for DMR

emissions %>%
    filter(source == 'DMR') %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
    mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')')) %>%
    group_by(`Chemical (CASRN)`, year, target_location) %>%
    summarize(load_kg = sum(load_kg)) %>%
    ungroup() %>%
    ggplot(aes(fill = `Chemical (CASRN)`, x = year, y = load_kg)) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_x_continuous(name = NULL, breaks = 2010:2022, labels = 2010:2022) +
    facet_wrap(.~target_location, scales = 'free_y') +
    labs(title = 'DMR emissions by year, chemical, and location',
         y = 'Emissions (kg)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave('figs/plots/DMR_emissions.png', width = 8, height = 8)

## bars for TRI

emissions %>%
    filter(source == 'TRI') %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
    mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')')) %>%
    group_by(`Chemical (CASRN)`, year, target_location) %>%
    summarize(load_kg = sum(load_kg)) %>%
    ungroup() %>%
    ggplot(aes(fill = `Chemical (CASRN)`, x = year, y = load_kg)) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_y_continuous(labels = label_number(suffix = ' K', scale = 1e-3, big.mark = '')) +
    scale_x_continuous(name = NULL, breaks = 2010:2022, labels = 2010:2022) +
    facet_wrap(.~target_location, scales = 'free_y') +
    labs(title = 'TRI emissions by year, chemical, and location',
         y = 'Emissions (kg)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave('figs/plots/TRI_emissions.png', width = 8, height = 8)

## bars for NEI

emissions %>%
    filter(source == 'NEI') %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
    mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')')) %>%
    group_by(`Chemical (CASRN)`, year, target_location) %>%
    summarize(load_kg = sum(load_kg)) %>%
    ungroup() %>%
    ggplot(aes(fill = `Chemical (CASRN)`, x = factor(year), y = load_kg)) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_y_continuous(labels = label_number(suffix = ' K', scale = 1e-3, big.mark = '')) +
    scale_x_discrete(name = NULL, breaks = as.character(2010:2022), labels = as.character(2010:2022)) +
    facet_wrap(.~target_location, scales = 'free_y') +
    labs(title = 'NEI emissions by year, chemical, and location',
         y = 'Emissions (kg)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave('figs/plots/NEI_emissions.png', width = 8, height = 8)

# grouped barplots of TRI emissions by release medium ####

## across chems

emissions %>%
    filter(source == 'TRI') %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
    mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')')) %>%
    group_by(`Chemical (CASRN)`, medium, target_location) %>%
    summarize(load_kg = sum(load_kg)) %>%
    ungroup() %>%
    ggplot(aes(x = `Chemical (CASRN)`, fill = medium, y = load_kg)) +
    geom_col(position = position_dodge2(width = 1, preserve = 'single')) +
    facet_wrap(.~target_location, scales = 'free') +
    labs(title = 'TRI emissions by release medium, chemical, and location',
         y = 'Emissions (kg)') +
    guides(x = guide_axis(angle = 55)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(hjust=0.3)) +
    scale_y_log10(breaks = c(0, 1, 10, 100, 1000, 10000, 1e5, 1e6, 1e7),
                  labels = c('0', '1', '10', '100', '1 K', '10 K', '100 K', '1 M', '10 M'))

ggsave('figs/plots/TRI_emissions_by_release_medium.png', width = 8, height = 8)


# grouped barplot of West Louisville Air Toxics Study ####

wlats = read_csv('data/west_louisville_air_toxics_study/release_concentrations.csv')

wlats %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
    mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')'),
           site = paste('Site', site)) %>%
    ggplot(aes(fill = `Chemical (CASRN)`, x = year, y = mean_conc_ugm3, color = `Chemical (CASRN)`)) +
    geom_col(position = position_dodge2(width = 1, preserve = 'single')) +
    geom_errorbar(aes(ymin = mean_conc_ugm3, ymax = mean_conc_ugm3+sd, color = `Chemical (CASRN)`), width = .2,
                  position = position_dodge(.9)) +
    facet_wrap(.~site, scales = 'fixed') +
    labs(title = 'West Louisville Air Toxics Study: measured concentrations at 6 sites over 5 years',
         y = expression('Mean concentration (' * mu * 'gm' ^-3 * ')' + '1 SD')) +
    guides(x = guide_axis(angle = 55)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(hjust=0.3))

ggsave('figs/plots/WLATS_concentrations.png', width = 8, height = 6)
