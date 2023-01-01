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
# library(VennDiagram)
# library(magick)
# library(htmlwidgets)
# library(htmltools)

sw = suppressWarnings
sm = suppressMessages

# setup ####

source('src/00_globals.R')

cas = read_csv('data/general/target_substances.csv', col_types=cols())

cities = read_csv('data/general/cities.csv', col_types=cols()) %>%
    mutate(county = clean_county_names(county))

houston_counties = cities %>%
    filter(! is.na(city) & city == 'HOUSTON') %>%
    pull(county)

emissions0 = read_csv('data/emissions_harmonized_epamethod_TRIpriority_2010-22.csv',
                     col_types = 'icciccnnnlclc') %>%
    mutate(load_lb = load_kg * 2.20462) %>%
    select(-load_kg) %>%
    relocate(load_lb, .after = medium) %>%
    mutate(inv_pref = 'TRI')

emissions_dmrnei_priority0 = read_csv('data/emissions_harmonized_epamethod_NEIDMRpriority_2010-22.csv',
                     col_types = 'icciccnnnlclc') %>%
    mutate(load_lb = load_kg * 2.20462) %>%
    select(-load_kg) %>%
    relocate(load_lb, .after = medium) %>%
    mutate(inv_pref = 'NEI-DMR')

# stewi's accounting for reporting overlap just removes non-preference inventories wholesale.
# fix that, assuming 100% overlap
cmb = bind_rows(emissions0, emissions_dmrnei_priority0) %>%
    mutate(source = tolower(source)) %>%
    unite('src_med', source, medium, inv_pref) %>%
    # select(-lat, -lon, -location_set_to_county_centroid, -illegal,
    #        -target_location) %>%
    distinct(year, state, county, cas, frs_id, src_med, load_lb,
             .keep_all = TRUE) %>%
    pivot_wider(names_from = src_med, values_from = load_lb)

#determined that there are no discrepancies within sources + across preferences
# cmb = cmb %>%
#     mutate(dmr_water_dif = dmr_water_TRI - `dmr_water_NEI-DMR`,
#            nei_air_dif = nei_air_TRI - `nei_air_NEI-DMR`,
#            tri_air_dif = tri_air_TRI - `tri_air_NEI-DMR`,
#            tri_water_dif = tri_water_TRI - `tri_water_NEI-DMR`,
#            tri_soil_dif = tri_soil_TRI - `tri_soil_NEI-DMR`)
# filter(cmb, if_any(ends_with('_dif'), ~ (! is.na(.) & . != 0))) %>%
#     select(year, state, county, cas, frs_id, ends_with('_dif'))
# zz = rowSums(select(cmb, ends_with('_dif')), na.rm = TRUE)
# table(zz)

fixed = cmb %>%
    mutate(
       water_dif = tri_water_TRI - `dmr_water_NEI-DMR`,
       air_dif = tri_air_TRI - `nei_air_NEI-DMR`)
    # filter((! is.na(water_dif) & water_dif != 0) |
    #            ((! is.na(air_dif) & air_dif != 0))) %>%
    # arrange(water_dif) %>%
    # slice(1:5) %>%
    # print() %>%

dmr_surplus = ! is.na(fixed$water_dif) & fixed$water_dif < 0
nei_surplus = ! is.na(fixed$air_dif) & fixed$air_dif < 0
tri_surplus = ! is.na(fixed$water_dif) & (fixed$water_dif > 0 | fixed$air_dif > 0)

fixed$dmr_water_TRI[dmr_surplus] = fixed$`dmr_water_NEI-DMR`[dmr_surplus] - fixed$tri_water_TRI[dmr_surplus]
fixed$dmr_water_TRI[tri_surplus] = 0
fixed$`tri_water_NEI-DMR`[dmr_surplus] = fixed$tri_water_TRI[dmr_surplus] - fixed$`dmr_water_NEI-DMR`[dmr_surplus]
fixed$`tri_water_NEI-DMR`[tri_surplus] = 0
fixed$nei_air_TRI[nei_surplus] = fixed$`nei_air_NEI-DMR`[nei_surplus] - fixed$tri_air_TRI[nei_surplus]
fixed$nei_air_TRI[tri_surplus] = 0
fixed$`tri_air_NEI-DMR`[nei_surplus] = fixed$tri_air_TRI[nei_surplus] - fixed$`nei_air_NEI-DMR`[nei_surplus]
fixed$`tri_air_NEI-DMR`[tri_surplus] = 0

fixed$dmrnei_surplus = dmr_surplus | nei_surplus
fixed$tri_surplus = tri_surplus

fixed = fixed %>%
    # mutate(
    #    water_dif = `tri_water_NEI-DMR` - dmr_water_TRI,
    #    air_dif = `tri_air_NEI-DMR` - nei_air_TRI)
    select(-water_dif, -air_dif) %>%
    pivot_longer(ends_with(c('DMR', 'TRI'), ignore.case = FALSE),
                 names_to = c('source', 'medium', 'inv_pref'),
                 names_sep = '_',
                 values_to = 'load_lb') %>%
    mutate(source = toupper(source)) %>%
    filter(! is.na(load_lb) & load_lb != 0)

emissions = fixed %>%
    filter(
        inv_pref == 'TRI' |
        (inv_pref == 'NEI-DMR' & dmrnei_surplus)) %>%
    select(-inv_pref)

emissions_dmrnei_priority = fixed %>%
    filter(
        inv_pref == 'NEI-DMR' |
        (inv_pref == 'TRI' & tri_surplus)) %>%
    select(-inv_pref)

sum(emissions0$load_lb)
sum(emissions$load_lb)
sum(emissions_dmrnei_priority0$load_lb)
sum(emissions_dmrnei_priority$load_lb)


# xx = anti_join(emissions_dmrnei_priority0, emissions0, by = c('year', 'state', 'county', 'cas', 'frs_id', 'medium', 'load_lb')) %>%
#     select(-location_set_to_county_centroid, -target_location, -illegal, -lat, -lon)
# yy = anti_join(emissions0, emissions_dmrnei_priority0, by = c('year', 'state', 'county', 'cas', 'frs_id', 'medium', 'load_lb')) %>%
#     select(-location_set_to_county_centroid, -target_location, -illegal, -lat, -lon)
# qq = full_join(xx, yy, by = c("year", "state", "county", "cas", "frs_id", "medium")) %>%
#     arrange(year, state, county, cas, frs_id)
#     # relocate(load_lb.x, .after='source')
# # filter(qq, !is.na(load_lb.x), !is.na(load_lb.y))
# mutate(qq, dif = load_lb.x - load_lb.y) %>% arrange(dif) %>% View()
#
# xx = select(emissions_dmrnei_priority0, -location_set_to_county_centroid, -target_location, -illegal, -lat, -lon)
# yy = select(emissions0, -location_set_to_county_centroid, -target_location, -illegal, -lat, -lon)
# qq = full_join(xx, yy, by = c("year", "state", "county", "cas", "frs_id", "medium")) %>%
#     arrange(year, state, county, cas, frs_id)
# qq = mutate(qq, dif = load_lb.x - load_lb.y) %>% arrange(dif)
# filter(qq, dif > 0) %>% arrange(desc(dif))
# filter(qq, dif == 0) %>% arrange(desc(dif))
#
# filter(qq, year == 2015, state == 'TX', county == 'HARRIS', cas == 106990, frs_id == 110031267064)
# filter(qq, year == 2015, state == 'TX', county == 'HARRIS', cas == 106990, frs_id == 110000463221)
#
# filter(xx, year == 2011, state == 'KY', county == 'JEFFERSON', cas == 50000, frs_id == 110000378467)
# filter(yy, year == 2011, state == 'KY', county == 'JEFFERSON', cas == 50000, frs_id == 110000378467)

emissions_11_18 = filter(emissions, year %in% 2011:2018)

sources = unique(emissions$source)

uniq_chems = emissions %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
    mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')')) %>%
    pull(`Chemical (CASRN)`) %>%
    unique() %>%
    sort()

colors = hue_pal(h.start = 30)(length(uniq_chems))

chems_and_colors = colors
names(chems_and_colors) = uniq_chems

#create directory structure for storing plots and maps
dir.create('figs/plots/', recursive = TRUE, showWarnings = FALSE)
dir.create('figs/fig_values/', recursive = TRUE, showWarnings = FALSE)
dir.create('figs/fig_values/map_values', recursive = TRUE, showWarnings = FALSE)
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
                summarize(load_lb = sum(load_lb, na.rm = TRUE),
                          .groups = 'drop')

            ej_map2_pointsize(
                ddo, center = map_center, scale = map_scale, res = 1/60,
                latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                title = glue('{sr}, {lc}: Combined emissions across {nc} of 24 high-priority chemicals, 2010-22',
                             sr = src, nc = length(unique(ddfilt$cas)), lc = loc),
                fileout = glue('figs/load_maps/by_source_and_location/{sr}_{lc}_allchems_allyears.html',
                               sr = tolower(src),
                               lc = gsub(' ', '_', tolower(loc))))

            ddo_rsei_f = ddfilt %>%
                left_join(select(cas, cas = CASRN_nohyphens, rsei_weight)) %>%
                filter(! is.na(rsei_weight))

            load_lb_rsei = ddo_rsei_f$load_lb * ddo_rsei_f$rsei_weight
            ddo_rsei_f$load_lb = load_lb_rsei / sum(load_lb_rsei) * sum(ddo_rsei_f$load_lb)

            ddo_rsei = ddo_rsei_f %>%
                group_by(lat, lon) %>%
                summarize(load_lb = sum(load_lb, na.rm = TRUE),
                          .groups = 'drop')

            if(nrow(ddo_rsei)){
                ej_map2_pointsize(
                    ddo_rsei, center = map_center, scale = map_scale, res = 1/60,
                    latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                    title = glue('{sr}, {lc}: RSEI-weighted emissions across {nc} of 24 high-priority chemicals, 2010-22',
                                 sr = src, nc = length(unique(ddo_rsei_f$cas)), lc = loc),
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
                    summarize(load_lb = sum(load_lb, na.rm = TRUE),
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

                    ddc_rsei_f = ddfilt2 %>%
                        left_join(select(cas, cas = CASRN_nohyphens, rsei_weight)) %>%
                        filter(! is.na(rsei_weight))

                    load_lb_rsei = ddc_rsei_f$load_lb * ddc_rsei_f$rsei_weight
                    ddc_rsei_f$load_lb = load_lb_rsei / sum(load_lb_rsei) * sum(ddc_rsei_f$load_lb)

                    ddc_rsei = ddc_rsei_f %>%
                        group_by(lat, lon) %>%
                        summarize(load_lb = sum(load_lb, na.rm = TRUE),
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
                            summarize(load_lb = sum(load_lb, na.rm = TRUE),
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

                            ddy1_rsei_f = ddfilt3 %>%
                                left_join(select(cas, cas = CASRN_nohyphens, rsei_weight)) %>%
                                filter(! is.na(rsei_weight))

                            load_lb_rsei = ddy1_rsei_f$load_lb * ddy1_rsei_f$rsei_weight
                            ddy1_rsei_f$load_lb = load_lb_rsei / sum(load_lb_rsei) * sum(ddy1_rsei_f$load_lb)

                            ddy1_rsei = ddy1_rsei_f %>%
                                group_by(lat, lon) %>%
                                summarize(load_lb = sum(load_lb, na.rm = TRUE),
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
                    summarize(load_lb = sum(load_lb, na.rm = TRUE),
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

                    ddy2_rsei_f = ddfilt4 %>%
                        left_join(select(cas, cas = CASRN_nohyphens, rsei_weight)) %>%
                        filter(! is.na(rsei_weight))

                    load_lb_rsei = ddy2_rsei_f$load_lb * ddy2_rsei_f$rsei_weight
                    ddy2_rsei_f$load_lb = load_lb_rsei / sum(load_lb_rsei) * sum(ddy2_rsei_f$load_lb)

                    ddy2_rsei = ddy2_rsei_f %>%
                        group_by(lat, lon) %>%
                        summarize(load_lb = sum(load_lb, na.rm = TRUE),
                                  .groups = 'drop')

                    if(nrow(ddy2_rsei)){
                        ej_map2_pointsize(
                            ddy2_rsei, center = map_center, scale = map_scale, res = 1/60,
                            latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                            title = glue('{sr}, {lc}: RSEI-weighted emissions across {nc} of 24 high-priority chemicals, {yr}',
                                         sr = src, nc = length(unique(ddy2_rsei_f$cas)), lc = loc, yr = yr),
                            fileout = glue('figs/rsei_maps/by_source_and_location/by_year/{sr}_{lc}_{yr}_allchems.html',
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
    summarize(load_lb = sum(load_lb)) %>%
    ungroup() %>%
    print(n = 100)

## stacked bars for combined emissions

ee = emissions %>%
    # filter(source != 'NPDES') %>%
    mutate(source = ifelse(source == 'NPDES', 'DMR', source)) %>%
    rename(Source = source) %>%
    # filter(county %in% houston_counties) %>%
    group_by(year, Source, target_location) %>%
    summarize(load_lb = sum(load_lb)) %>%
    ungroup()
write_csv(ee, 'figs/fig_values/combined_emissions.csv')
ee %>%
    ggplot(aes(fill = Source, x = year, y = load_lb)) +
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
         y = 'Emissions (lb)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    # scale_fill_viridis_d()

ggsave('figs/plots/combined_emissions.png', width = 8, height = 8)

## bars for NPDES

ee = emissions %>%
    filter(source == 'NPDES') %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
    mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')'))
ee2 = ee %>%
    group_by(`Chemical (CASRN)`, year, target_location) %>%
    summarize(load_lb = sum(load_lb)) %>%
    ungroup()
write_csv(ee2, 'figs/fig_values/illegal_emissions.csv')
ee2 %>%
    ggplot(aes(fill = `Chemical (CASRN)`, x = year, y = load_lb)) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_x_continuous(name = NULL, breaks = 2010:2022, labels = 2010:2022) +
    facet_wrap(.~target_location, scales = 'free_y') +
    labs(title = 'NPDES illegal emissions by year, chemical, and location',
         y = 'Emissions (lb)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual(name = 'Chemical (CASRN)',
                      breaks = uniq_chems[uniq_chems %in% unique(ee$`Chemical (CASRN)`)],
                      values = chems_and_colors[names(chems_and_colors) %in% unique(ee$`Chemical (CASRN)`)])

ggsave('figs/plots/illegal_emissions.png', width = 8, height = 4)

## bars for DMR

ee = emissions_dmrnei_priority %>%
    mutate(source = ifelse(source == 'NPDES', 'DMR', source)) %>%
    filter(source == 'DMR') %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
    mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')'))
ee2 = ee %>%
    group_by(`Chemical (CASRN)`, year, target_location) %>%
    summarize(load_lb = sum(load_lb)) %>%
    ungroup()
write_csv(ee2, 'figs/fig_values/DMR_emissions.csv')
ee2 %>%
    ggplot(aes(fill = `Chemical (CASRN)`, x = year, y = load_lb)) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_x_continuous(name = NULL, breaks = 2010:2022, labels = 2010:2022) +
    facet_wrap(.~target_location, scales = 'free_y') +
    labs(title = 'DMR emissions by year, chemical, and location',
         y = 'Emissions (lb)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual(name = 'Chemical (CASRN)',
                      breaks = uniq_chems[uniq_chems %in% unique(ee$`Chemical (CASRN)`)],
                      values = chems_and_colors[names(chems_and_colors) %in% unique(ee$`Chemical (CASRN)`)])

ggsave('figs/plots/DMR_emissions.png', width = 10, height = 5)

## bars for TRI

ee = emissions %>%
    filter(source == 'TRI') %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
    mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')'))
ee2 = ee %>%
    group_by(`Chemical (CASRN)`, year, target_location) %>%
    summarize(load_lb = sum(load_lb)) %>%
    ungroup()
write_csv(ee2, 'figs/fig_values/TRI_emissions.csv')
ee2 %>%
    ggplot(aes(fill = `Chemical (CASRN)`, x = year, y = load_lb)) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_y_continuous(labels = label_number(suffix = ' K', scale = 1e-3, big.mark = '')) +
    scale_x_continuous(name = NULL, breaks = 2010:2022, labels = 2010:2022) +
    facet_wrap(.~target_location, scales = 'free_y') +
    labs(title = 'TRI emissions by year, chemical, and location',
         y = 'Emissions (lb)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual(name = 'Chemical (CASRN)',
                      breaks = uniq_chems[uniq_chems %in% unique(ee$`Chemical (CASRN)`)],
                      values = chems_and_colors[names(chems_and_colors) %in% unique(ee$`Chemical (CASRN)`)])

ggsave('figs/plots/TRI_emissions.png', width = 8, height = 8)

## bars for NEI

ee = emissions_dmrnei_priority %>%
    filter(source == 'NEI') %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
    mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')'))
ee2 = ee %>%
    group_by(`Chemical (CASRN)`, year, target_location) %>%
    summarize(load_lb = sum(load_lb)) %>%
    ungroup()
write_csv(ee2, 'figs/fig_values/NEI_emissions.csv')
ee2 %>%
    ggplot(aes(fill = `Chemical (CASRN)`, x = factor(year), y = load_lb)) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_y_continuous(labels = label_number(suffix = ' K', scale = 1e-3, big.mark = '')) +
    scale_x_discrete(name = NULL, breaks = as.character(2010:2022), labels = as.character(2010:2022)) +
    facet_wrap(.~target_location, scales = 'free_y') +
    labs(title = 'NEI emissions by year, chemical, and location',
         y = 'Emissions (lb)') +
    guides(x = guide_axis(angle = 45)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual(name = 'Chemical (CASRN)',
                      breaks = uniq_chems[uniq_chems %in% unique(ee$`Chemical (CASRN)`)],
                      values = chems_and_colors[names(chems_and_colors) %in% unique(ee$`Chemical (CASRN)`)])

ggsave('figs/plots/NEI_emissions.png', width = 8, height = 8)

# grouped barplots of TRI emissions by release medium ####

## across chems

ee = emissions %>%
    filter(source == 'TRI', cas != 79947) %>% #TBBPA is around 0.01 lb and messes up the plots
    filter(! (target_location == 'Cancer Alley' & cas == 106990 & medium == 'water')) %>% #same
    left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
    mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')')) %>%
    group_by(`Chemical (CASRN)`, medium, target_location) %>%
    summarize(load_lb = sum(load_lb)) %>%
    ungroup()
write_csv(ee, 'figs/fig_values/TRI_emissions_by_release_medium.csv')
ee %>%
    ggplot(aes(x = `Chemical (CASRN)`, fill = medium, y = load_lb)) +
    geom_col(position = position_dodge2(width = 1, preserve = 'single')) +
    facet_wrap(.~target_location, scales = 'free') +
    labs(title = 'TRI emissions by release medium, chemical, and location',
         y = 'Emissions (lb)') +
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

# venn diagram of chems by source ####

#the VennDiagram package doesn't produce the kind of diagram we want,
#so i made that in powerpoint. The code below does all the summarizing you
#need to make a venn diagram yourself

sources_by_chem = emissions %>%
    left_join(select(cas, cas=CASRN_nohyphens, ej_name)) %>%
    group_by(ej_name) %>%
    summarize(ej_name = first(ej_name),
              sources = list(unique(source)),
              .groups = 'drop') %>%
    as.data.frame() %>%
    split(1:nrow(.))

chem_list = lapply(sources_by_chem, function(x) as.list(x)$sources[[1]])
names(chem_list) = sapply(sources_by_chem, function(x) as.list(x)$ej_name)

# unq_chms = emissions %>%
#     left_join(select(cas, cas=CASRN_nohyphens, ej_name)) %>%
#     group_by(source) %>%
#     summarize(source = first(source),
#               chems = list(unique(ej_name)),
#               .groups = 'drop') %>%
#     as.data.frame() %>%
#     split(1:nrow(.))
#
# chem_list = lapply(unq_chms, function(x) as.list(x)$chems[[1]])
# names(chem_list) = sapply(unq_chms, function(x) as.list(x)$source)

# allchems = Reduce(union, chem_list)

# venn.diagram(
#     x = chem_list,
#     # category.names = c("Set 1" , "Set 2 " , "Set 3"),
#     main = 'Chemicals reported by each source across target locations and time range',
#     imagetype = 'png',
#     filename = 'figs/plots/chem_venndiagram.png',
#     output=TRUE,
#     disable.logging = TRUE
# )

# emissions and RSEI maps. static, final v1. ####

sources = list('TRI', c('DMR', 'NEI', 'NPDES'))

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

    expand = 0.02
    latrange = range(dd$lat, na.rm = TRUE)
    latrange[1] = latrange[1] - expand
    latrange[2] = latrange[2] + expand
    lonrange = range(dd$lon, na.rm = TRUE)
    lonrange[1] = lonrange[1] - expand
    lonrange[2] = lonrange[2] + expand

    if(nrow(dd)){

        for(src in sources){

            ddfilt = filter(dd, source %in% !!src)

            ddo = ddfilt %>%
                group_by(lat, lon) %>%
                summarize(load_lb = sum(load_lb, na.rm = TRUE),
                          .groups = 'drop')

            ej_map2_pointsize(
                ddo, center = map_center, scale = map_scale, res = 1/60,
                latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                title = glue('{sr}, {lc}: Combined emissions across {nc} of 24 high-priority chemicals, 2010-22',
                             sr = ifelse(src[[1]] == 'TRI', src, 'NEI+DMR+violations'),
                             nc = length(unique(ddfilt$cas)), lc = loc),
                fileout = glue('figs/plots/final_maps/{sr}_{lc}_load.png',
                               sr = ifelse(src[[1]] == 'TRI', tolower(src), 'nei-dmr'),
                               lc = gsub(' ', '_', tolower(loc))),
                plot_legend = FALSE, plot_title = FALSE, plot_locations = FALSE)

            ddo_rsei_f = ddfilt %>%
                left_join(select(cas, cas = CASRN_nohyphens, rsei_weight)) %>%
                filter(! is.na(rsei_weight))

            load_lb_rsei = ddo_rsei_f$load_lb * ddo_rsei_f$rsei_weight
            ddo_rsei_f$load_lb = load_lb_rsei / sum(load_lb_rsei) * sum(ddo_rsei_f$load_lb)

            ddo_rsei = ddo_rsei_f %>%
                group_by(lat, lon) %>%
                summarize(load_lb = sum(load_lb, na.rm = TRUE),
                          .groups = 'drop')

            if(nrow(ddo_rsei)){
                ej_map2_pointsize(
                    ddo_rsei, center = map_center, scale = map_scale, res = 1/60,
                    latrange = latrange, lonrange = lonrange, addpoints = TRUE,
                    title = glue('{sr}, {lc}: RSEI-weighted emissions across {nc} of 24 high-priority chemicals, 2010-22',
                                 sr = ifelse(src[[1]] == 'TRI', src, 'NEI+DMR+violations'),
                                 nc = length(unique(ddo_rsei_f$cas)), lc = loc),
                    fileout = glue('figs/plots/final_maps/{sr}_{lc}_rsei.png',
                                   sr = ifelse(src[[1]] == 'TRI', tolower(src), 'nei-dmr'),
                                   lc = gsub(' ', '_', tolower(loc))),
                    type = 'rsei', plot_locations = FALSE,
                    plot_legend = FALSE, plot_title = FALSE, point_color = 'green')
            }
        }
    }
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

# emissions maps. static, final v2-3. ####

# sources = list('TRI', c('DMR', 'NEI', 'NPDES'))

file.create('figs/plots/facility_counts.txt')
dir.create('figs/plots/final_map_chemlists')
file.create('figs/plots/final_map_chemlists/tri_only_louisville.txt')

for(loc in unique(emissions_11_18$target_location)){

    dd = filter(emissions_11_18, target_location == loc)

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

    expand = 0.02
    latrange = range(dd$lat, na.rm = TRUE)
    latrange[1] = latrange[1] - expand
    latrange[2] = latrange[2] + expand
    lonrange = range(dd$lon, na.rm = TRUE)
    lonrange[1] = lonrange[1] - expand
    lonrange[2] = lonrange[2] + expand

    #tri-only
    src = 'TRI'
    ddfilt = filter(dd, source %in% !!src)

    ddfilt %>%
        group_by(cas) %>%
        summarize(load_lb = sum(load_lb, na.rm = TRUE)) %>%
        ungroup() %>%
        left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
        mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')')) %>%
        select(`Chemical (CASRN)`, load_lb) %>%
        write_csv(glue('figs/fig_values/map_values/tri_{lc}_trichems.csv',
                       lc = sub(' ', '_', tolower(loc))))

    tri_cas = unique(ddfilt$cas)
    tri_chem = filter(cas, CASRN_nohyphens %in% tri_cas) %>% pull(ej_name)
    # print(glue('{src}, {loc} ({cnt}): {chm}',
    #            cnt = length(tri_cas),
    #            chm = paste(tri_chem, collapse = ', ')))
    print(glue('{paste(src, collapse = ", ")}, {loc} ({cnt})', cnt = length(tri_cas)))
    write_lines(tri_chem,
                glue('figs/plots/final_map_chemlists/tri_only_{lc}.txt',
                     lc = sub(' ', '_', tolower(loc))))

    ddo = ddfilt %>%
        group_by(lat, lon) %>%
        summarize(load_lb = sum(load_lb, na.rm = TRUE),
                  .groups = 'drop')

    write_lines(glue('{loc}, TRI, TRI chems: {nrow(ddo)} facilities'),
                glue('figs/plots/facility_counts.txt'), append = TRUE)

    ej_map2_pointsize(
        ddo, center = map_center, scale = map_scale, res = 1/60,
        latrange = latrange, lonrange = lonrange, addpoints = TRUE,
        title = '', point_color = 'blue',
        fileout = glue('figs/plots/final_maps/tri_{lc}_trichems.png',
                       lc = sub(' ', '_', tolower(loc))),
        plot_legend = FALSE, plot_title = FALSE, plot_locations = FALSE)

    #nei+dmr: tri chems
    src = c('DMR', 'NEI', 'NPDES')
    ddfilt = filter(dd, source %in% !!src)
    ddfilt = filter(dd, cas %in% tri_cas)

    ddfilt %>%
        group_by(cas) %>%
        summarize(load_lb = sum(load_lb, na.rm = TRUE)) %>%
        ungroup() %>%
        left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
        mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')')) %>%
        select(`Chemical (CASRN)`, load_lb) %>%
        write_csv(glue('figs/fig_values/map_values/dmr_nei_{lc}_trichems.csv',
                       lc = sub(' ', '_', tolower(loc))))

    unique_cas = unique(ddfilt$cas)
    unique_chem = filter(cas, CASRN_nohyphens %in% unique_cas) %>% pull(ej_name)
    # print(glue('{src}, {loc} ({cnt}): {chm}',
    #            cnt = length(unique_cas),
    #            chm = paste(unique_chem, collapse = ', ')))
    print(glue('{paste(src, collapse = ", ")}, {loc} ({cnt})', cnt = length(unique_cas)))

    ddo = ddfilt %>%
        group_by(lat, lon) %>%
        summarize(load_lb = sum(load_lb, na.rm = TRUE),
                  .groups = 'drop')

    write_lines(glue('{loc}, DMR+NEI, TRI chems: {nrow(ddo)} facilities'),
                glue('figs/plots/facility_counts.txt'), append = TRUE)

    ej_map2_pointsize(
        ddo, center = map_center, scale = map_scale, res = 1/60,
        latrange = latrange, lonrange = lonrange, addpoints = TRUE,
        title = '', point_color = 'blue',
        fileout = glue('figs/plots/final_maps/dmr_nei_{lc}_trichems.png',
                       lc = sub(' ', '_', tolower(loc))),
        plot_legend = FALSE, plot_title = FALSE, plot_locations = FALSE)

    #nei+dmr: non-tri chems
    src = c('DMR', 'NEI', 'NPDES')
    ddfilt = filter(dd, source %in% !!src)
    ddfilt = filter(dd, ! cas %in% tri_cas)

    ddfilt %>%
        group_by(cas) %>%
        summarize(load_lb = sum(load_lb, na.rm = TRUE)) %>%
        ungroup() %>%
        left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
        mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')')) %>%
        select(`Chemical (CASRN)`, load_lb) %>%
        write_csv(glue('figs/fig_values/map_values/dmr_nei_{lc}_nontrichems.csv',
                       lc = sub(' ', '_', tolower(loc))))

    unique_cas = unique(ddfilt$cas)
    unique_chem = filter(cas, CASRN_nohyphens %in% unique_cas) %>% pull(ej_name)
    # print(glue('{src}, {loc} ({cnt}): {chm}',
    #            cnt = length(unique_cas),
    #            chm = paste(unique_chem, collapse = ', ')))
    print(glue('{paste(src, collapse = ", ")}, {loc} ({cnt})', cnt = length(unique_cas)))
    write_lines(unique_chem,
                glue('figs/plots/final_map_chemlists/non-tri_{lc}.txt',
                     lc = sub(' ', '_', tolower(loc))))

    ddo = ddfilt %>%
        group_by(lat, lon) %>%
        summarize(load_lb = sum(load_lb, na.rm = TRUE),
                  .groups = 'drop')

    write_lines(glue('{loc}, DMR+NEI, non-TRI chems: {nrow(ddo)} facilities'),
                glue('figs/plots/facility_counts.txt'), append = TRUE)

    ej_map2_pointsize(
        ddo, center = map_center, scale = map_scale, res = 1/60,
        latrange = latrange, lonrange = lonrange, addpoints = TRUE,
        title = '', point_color = 'red',
        fileout = glue('figs/plots/final_maps/dmr_nei_{lc}_nontrichems.png',
                       lc = sub(' ', '_', tolower(loc))),
        plot_legend = FALSE, plot_title = FALSE, plot_locations = FALSE)

    #nei+dmr: all chems
    src = c('DMR', 'NEI', 'NPDES')
    ddfilt = filter(dd, source %in% !!src)

    ddfilt %>%
        group_by(cas) %>%
        summarize(load_lb = sum(load_lb, na.rm = TRUE)) %>%
        ungroup() %>%
        left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
        mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')')) %>%
        select(`Chemical (CASRN)`, load_lb) %>%
        write_csv(glue('figs/fig_values/map_values/dmr_nei_{lc}_allchems.csv',
                       lc = sub(' ', '_', tolower(loc))))

    unique_cas = unique(ddfilt$cas)
    unique_chem = filter(cas, CASRN_nohyphens %in% unique_cas) %>% pull(ej_name)
    # print(glue('{src}, {loc} ({cnt}): {chm}',
    #            cnt = length(unique_cas),
    #            chm = paste(unique_chem, collapse = ', ')))
    print(glue('{paste(src, collapse = ", ")}, {loc} ({cnt})', cnt = length(unique_cas)))

    ddo = ddfilt %>%
        group_by(lat, lon) %>%
        summarize(load_lb = sum(load_lb, na.rm = TRUE),
                  .groups = 'drop')

    write_lines(glue('{loc}, DMR+NEI, all chems: {nrow(ddo)} facilities'),
                glue('figs/plots/facility_counts.txt'), append = TRUE)

    ej_map2_pointsize(
        ddo, center = map_center, scale = map_scale, res = 1/60,
        latrange = latrange, lonrange = lonrange, addpoints = TRUE,
        title = '', point_color = 'purple',
        fileout = glue('figs/plots/final_maps/dmr_nei_{lc}_allchems.png',
                       lc = sub(' ', '_', tolower(loc))),
        plot_legend = FALSE, plot_title = FALSE, plot_locations = FALSE)

    #all sources, all chems
    src = c('TRI', 'DMR', 'NEI', 'NPDES')
    ddfilt = filter(dd, source %in% !!src)

    ddfilt %>%
        group_by(cas) %>%
        summarize(load_lb = sum(load_lb, na.rm = TRUE)) %>%
        ungroup() %>%
        left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
        mutate(`Chemical (CASRN)` = paste0(ej_name, ' (', CASRN, ')')) %>%
        select(`Chemical (CASRN)`, load_lb) %>%
        write_csv(glue('figs/fig_values/map_values/tri_dmr_nei_{lc}_allchems.csv',
                       lc = sub(' ', '_', tolower(loc))))

    unique_cas = unique(ddfilt$cas)
    unique_chem = filter(cas, CASRN_nohyphens %in% unique_cas) %>% pull(ej_name)
    # print(glue('{src}, {loc} ({cnt}): {chm}',
    #            cnt = length(unique_cas),
    #            chm = paste(unique_chem, collapse = ', ')))
    print(glue('{paste(src, collapse = ", ")}, {loc} ({cnt})', cnt = length(unique_cas)))

    ddo = ddfilt %>%
        group_by(lat, lon) %>%
        summarize(load_lb = sum(load_lb, na.rm = TRUE),
                  .groups = 'drop')

    write_lines(glue('{loc}, TRI+DMR+NEI, all chems: {nrow(ddo)} facilities'),
                glue('figs/plots/facility_counts.txt'), append = TRUE)

    ej_map2_pointsize(
        ddo, center = map_center, scale = map_scale, res = 1/60,
        latrange = latrange, lonrange = lonrange, addpoints = TRUE,
        title = '', point_color = 'purple',
        fileout = glue('figs/plots/final_maps/tri_dmr_nei_{lc}_allchems.png',
                       lc = sub(' ', '_', tolower(loc))),
        plot_legend = FALSE, plot_title = FALSE, plot_locations = FALSE)
}

# some quick numbers ####

#tri chems by location
tri_chems = emissions_11_18 %>%
    filter(source == 'TRI') %>%
    group_by(target_location) %>%
    summarize(cas = unique(cas)) %>%
    ungroup() %>%
    mutate(indicator = 1)

#tri load by location
emissions_11_18 %>%
    filter(source == 'TRI') %>%
    group_by(target_location) %>%
    summarize(load_lb = sum(load_lb),
              nchems = length(unique(cas))) %>%
    ungroup()

#non-tri load by location (for tri chems)
emissions_11_18 %>%
    filter(source != 'TRI') %>%
    left_join(tri_chems, by = c('target_location', 'cas')) %>%
    filter(! is.na(indicator)) %>%
    group_by(target_location) %>%
    # filter(cas %in% filter(tri_chems, target_location
    summarize(load_lb = sum(load_lb),
              nchems = length(unique(cas))) %>%
    ungroup()

#non-tri load by location
emissions_11_18 %>%
    filter(source != 'TRI') %>%
    group_by(target_location) %>%
    summarize(load_lb = sum(load_lb),
              nchems = length(unique(cas))) %>%
    ungroup()

#tri chems by location
emissions_11_18 %>%
    filter(source == 'TRI') %>%
    group_by(target_location) %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name), by = 'cas') %>%
    summarize(chems = paste(unique(ej_name), collapse = ', ')) %>%
    ungroup() %>%
    as.data.frame()

#tri chems reported to NEI and DMR by location
emissions_11_18 %>%
    filter(source != 'TRI') %>%
    left_join(tri_chems, by = c('target_location', 'cas')) %>%
    filter(! is.na(indicator)) %>%
    group_by(target_location) %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name), by = 'cas') %>%
    summarize(chems = paste(unique(ej_name), collapse = ', ')) %>%
    ungroup() %>%
    as.data.frame()

#all chems reported to NEI and DMR by location
emissions_11_18 %>%
    filter(source != 'TRI') %>%
    group_by(target_location) %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name), by = 'cas') %>%
    summarize(chems = paste(unique(ej_name), collapse = ', ')) %>%
    ungroup() %>%
    as.data.frame()

#tri total load
emissions_11_18 %>%
    filter(source == 'TRI') %>%
    summarize(load_lb = sum(load_lb),
              nchems = length(unique(cas)))

#non-tri total load for tri chems
emissions_11_18 %>%
    filter(source != 'TRI') %>%
    left_join(tri_chems, by = c('target_location', 'cas')) %>%
    filter(! is.na(indicator)) %>%
    summarize(load_lb = sum(load_lb),
              nchems = length(unique(cas)))

#non-tri total load
emissions_11_18 %>%
    filter(source != 'TRI') %>%
    summarize(load_lb = sum(load_lb),
              nchems = length(unique(cas)))

#port arthur formaldehyde
emissions %>%
    filter(source != 'TRI',
           cas == 50000,
           target_location == 'Port Arthur') %>%
    summarize(load_lb = sum(load_lb))

#tri facility counts
emissions_11_18 %>%
    filter(source == 'TRI') %>%
    distinct(target_location, lat, lon) %>%
    group_by(target_location) %>%
    summarize(n = n()) %>%
    ungroup()

#non-tri facility counts
emissions_11_18 %>%
    filter(source != 'TRI') %>%
    distinct(target_location, lat, lon) %>%
    group_by(target_location) %>%
    summarize(n = n()) %>%
    ungroup()

#release volumes by chem, location, and source (2011-2018)

emissions_11_18 %>%
    mutate(source = ifelse(source %in% c('DMR', 'NEI', 'NPDES'), 'DMR-NEI', 'TRI')) %>%
    left_join(select(cas, cas = CASRN_nohyphens, ej_name, CASRN)) %>%
    group_by(ej_name, source, target_location) %>%
    summarize(load_lb = sum(load_lb)) %>%
    ungroup() %>%
    pivot_wider(names_from = source, values_from = load_lb) %>%
    arrange(target_location, ej_name) %>%
    relocate(target_location, .before = ej_name) %>%
    print(n = 100)

# gifs? ####

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

# compare target locations to country at large ####

