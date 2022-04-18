get_substance_by_cas = function(casrn_vec,
                                exclude_synonyms = TRUE,
                                include_former_and_incorrect_cas = FALSE,
                                include_related = FALSE){

    query_base = 'https://cdxnodengn.epa.gov/cdx-srs-rest/substances/cas?casList='

    casrn_str = casrn_vec %>%
        as.character() %>%
        str_replace('-', '') %>%
        paste(collapse='%7C')

    excl_syn = ifelse(exclude_synonyms, '&excludeSynonyms=true', '')
    incl_other = ifelse(include_former_and_incorrect_cas, '&includeOtherCas=true', '')
    incl_rel = ifelse(include_related, '&includeOtherTsn=true', '')

    query = glue('{qb}{cc}{es}{io}{ir}&qualifier=exact',
                 qb = query_base,
                 cc = casrn_str,
                 es = excl_syn,
                 io = incl_other,
                 ir = incl_rel)

    r = httr::GET(query)
    d = httr::content(r, as='text', encoding='UTF-8')
    d = jsonlite::fromJSON(d)

    return(d)
}

get_ef_tablechunk = function(table_name,
                             column_name=NULL,
                             operator=NULL,
                             column_value=NULL,
                             rows=NULL,
                             rtn_fmt,
                             timeout_,
                             timeout_action,
                             debug_){

    # table_name: char string; the name of an envirofacts table
    # column_name: char: optional; use to filter by column value (see operator)
    # operator: =, !=, <, >, BEGINNING, CONTAINING; if filtering, the filter expression's operator (with column_value)
    # column_value: if filtering, the value on the RHS of the filtering expression
    # rows: 'x:y'; max of 100,000
    # see query_ef_table

    query_base = 'https://data.epa.gov/efservice/'
    # query_base = 'http://iaspub.epa.gov/enviro/efservice/' #old? still in one of their examples

    table_names_ext = ''
    if(length(table_name) > 1) stop('table_name max length is 1 (maybe raise an issue with EPA about this? should be allowed length 3)')
    # if(length(table_names) > 1){ #deprecated for now
    #     table_names_ext = paste0('/',
    #                              paste(table_names[2:length(table_names)],
    #                                    collapse = '/'))
    #     table_names = table_names[1]
    # }

    if(! is.null(column_name)){

        cl = length(column_name)
        if(length(operator) != cl && length(column_value) != cl){
            stop('lengths of column_name, operator, and column_value must all be equal')
        }

        filter_str = ''
        for(i in seq_len(cl)){
            filter_str = paste(filter_str,
                               paste(column_name[i], operator[i], column_value[i],
                                     sep = '/'),
                               sep = '/')
        }
    } else {
        filter_str = ''
    }

    # query = 'https://data.epa.gov/efservice/ICIS_LIMIT/REF_POLLUTANT/SRS_ID/439216/rows/1:5/JSON'
    # query='https://data.epa.gov/efservice/ICIS_LIMIT/SRS_ID/=/5520/csv' ##***
    # query = glue('{qb}{tn}{cn}{op}{cv}{tnx}{ro}/csv',
    query = glue('{qb}{tn}{fs}{ro}/{fmt}',
                 qb=query_base,
                 tn=paste(table_name, collapse='/'),
                 # cn=ifelse(is.null(column_name), '', paste0('/', column_name)),
                 # op=ifelse(is.null(operator), '', paste0('/', operator)),
                 # cv=ifelse(is.null(column_value), '', paste0('/', column_value)),
                 # tnx=table_names_ext,
                 fs=filter_str,
                 ro=ifelse(! is.null(rows), paste0('/rows/', rows), ''),
                 fmt=rtn_fmt)

    if(debug_) browser()

    # query = 'https://data.epa.gov/efservice/FACILITIES/rows/40000:49999/csv'
    # query = 'https://data.epa.gov/efservice/FACILITIES/rows/50000:59999/csv'
    # query = 'https://data.epa.gov/efservice/FACILITIES/rows/18000:18010/csv'
    r = try(httr::GET(query, timeout(timeout_)),
            silent = TRUE)
    if(inherits(r, 'try-error')){
        if(timeout_action == 'skip' && grepl('Timeout was reached', r[[1]])){
            message('timout reached. skipping this chunk. see timeout_ parameter.')
            return(tibble())
        }
        stop('timout reached. try increasing the value passed to the timeout_ parameter')
    }
    d = httr::content(r, as='text', encoding='UTF-8')
    # sw(sm(read_csv(d, col_types = cols(.default = 'c')))) %>%
    #     select(-starts_with('...')) %>%
    #     rename_with(function(x) str_match(x, '\\.([^\\.]+)$')[, 2])

    #sometimes SQL returns a server-side error when csv or json is requested.
    #so, trying csv first, and json if that fails.

    if(rtn_fmt == 'csv'){
        dout <- try({
            sw(sm(read_csv(d, col_types = cols(.default = 'c')))) %>%
            select(-starts_with('...')) %>%
            rename_with(function(x) str_match(x, '\\.([^\\.]+)$')[, 2])
        }, silent = TRUE)
    }

    if(rtn_fmt == 'json' || inherits(dout, 'try-error')){

        query = sub('csv$', 'json', query)
        r = try(httr::GET(query, timeout(timeout_)),
                silent = TRUE)
        if(inherits(r, 'try-error')){
            if(timeout_action == 'skip' && grepl('Timeout was reached', r[[1]])){
                message('timout reached. skipping this chunk. see timeout_ parameter.')
                return(tibble())
            }
            stop('timout reached. try increasing the value passed to the timeout_ parameter')
        }
        d = httr::content(r, as='text', encoding='UTF-8')
        dout = as_tibble(jsonlite::fromJSON(d)) %>%
            mutate(across(where(~! is.character(.)), as.character))
    }

    return(dout)
}

get_response_1char = function(msg,
                              possible_chars,
                              subsequent_prompt = FALSE){

    #msg: character. a message that will be used to prompt the user
    #possible_chars: character vector of acceptable single-character responses
    #subsequent prompt: not to be set directly. This is handled by
    #   get_response_1char during recursion.

    if(subsequent_prompt){
        cat(paste('Please choose one of:',
                  paste(possible_chars,
                        collapse = ', '),
                  '\n> '))
    } else {
        cat(msg)
    }

    ch <- as.character(readLines(con = stdin(), 1))

    if(length(ch) == 1 && ch %in% possible_chars){
        return(ch)
    } else {
        get_response_1char(msg = msg,
                           possible_chars = possible_chars,
                           subsequent_prompt = TRUE)
    }
}

# nrows=1; maxrows=1e5
# nrows=259000; maxrows=1e4
get_chunksets = function(nrows, maxrows){

    # nrows: the number of rows in an envirofacts table. probably queried via query_ef_rows()
    # maxrows: the maximum number of rows that can be returned via the envirofacts REST-API

    options(scipen = 100)

    nchunks = nrows %/% maxrows
    rem = nrows %% maxrows
    if(rem > 0) nchunks = nchunks + 1

    chunk_starts = seq(0, nrows, maxrows)
    chunk_ends = 1:nchunks * maxrows - 1
    if(rem != 0){
        chunk_ends[nchunks] = chunk_starts[nchunks] + rem
    }

    chunk_sets = paste(chunk_starts, chunk_ends,
                       sep=':')

    options(scipen = 0)

    return(chunk_sets)
}

query_ef_rows = function(table_name,
                         column_name=NULL,
                         operator=NULL,
                         column_value=NULL){

    # table_name: char string; the name of an envirofacts table
    # column_name: char: optional; use to filter by column value (see operator)
    # operator: =, !=, <, >, BEGINNING, CONTAINING; if filtering, the filter expression's operator (with column_value)
    # column_value: if filtering, the value on the RHS of the filtering expression

    query_base = 'https://data.epa.gov/efservice/'

    if(length(table_name) > 1) stop('this function is for querying rows of a single table')

    if(! is.null(column_name)){

        cl = length(column_name)
        if(length(operator) != cl && length(column_value) != cl){
            stop('lengths of column_name, operator, and column_value must all be equal')
        }

        filter_str = ''
        for(i in seq_len(cl)){
            filter_str = paste(filter_str,
                               paste(column_name[i], operator[i], column_value[i],
                                     sep = '/'),
                               sep = '/')
        }
    } else {
        filter_str = ''
    }

    # query = glue('{qb}{tn}{cn}{op}{cv}/count/json',
    query = glue('{qb}{tn}{fs}/count/json',
                 qb=query_base,
                 tn=paste(table_name, collapse='/'),
                 fs = filter_str)
                 # cn=ifelse(is.null(column_name), '', paste0('/', column_name)),
                 # op=ifelse(is.null(operator), '', paste0('/', operator)),
                 # cv=ifelse(is.null(column_value), '', paste0('/', column_value)))

    r = httr::GET(query)
    d = httr::content(r, as='text', encoding='UTF-8')
    d = jsonlite::fromJSON(d)[[1]]

    return(d)
}

query_ef_table = function(table_name,
                          column_name=NULL,
                          operator=NULL,
                          column_value=NULL,
                          specify_rows=TRUE,
                          chunk_size=1e4,
                          custom_chunks=NULL,
                          rtn_fmt='csv',
                          timeout_=120,
                          timeout_action='fail',
                          warn=TRUE,
                          verbose=FALSE,
                          debug_=FALSE){

    # table_name: char string; the name of an envirofacts table
    # column_name: char: optional; use to filter by column value (see operator)
    #   if filtering by multiple columns, include them all here as a character vector.
    # operator: =, !=, <, >, BEGINNING, CONTAINING; if filtering, the filter
    #   expression's operator (with column_value). If filtering on multiple columns,
    #   must supply a separate operator for each column as a character vector.
    #   Make sure elements of column_name, operator, and column_value line
    #   up correctly.
    # column_value: if filtering, the value on the RHS of the filtering expression.
    #   If filtering on multiple columns, must supply a value for each column as
    #   a vector. Make sure elements of column_name, operator, and column_value line
    #   up correctly.
    # specify_rows: logical. if TRUE, the API request will include "/rows/x:y".
    #   There's no reason to change the default unless you experience hanging requests
    #   or unexplainable request failures. Something might be misspecified server-
    #   side, and this could help. Note though that a request can't be larger
    #   than 100,000 rows. If your query would return more than this,
    #   specify_rows must be TRUE.
    # chunk_size: the maximum number of rows to ask for at a time. Envirofacts
    #   won't let you ask for more than 100,000 (the default).
    # custom_chunks: if all else fails, you can figure out exactly which chunks are
    #   causing trouble and tell get_ef_tablechunk exactly how to split
    #   up its requests. Amazingly, there are some chunks that can't be
    #   retrieved whole, but can be retrieved as two separate requests, notably
    #   https://data.epa.gov/efservice/FACILITIES/rows/31500:31600/csv will fail,
    #   but 31500:31550 and 31550:31600 will both work. or, 31500:31600/json will
    #   work. if specified, this argument overrides chunk_size.
    # rtn_fmt: character. semi-deprecated. This function will now try to return
    #   CSV results first, and if that fails, it'll try again with JSON results.
    #   You may also specify "json" to try that first, but then it won't fall back to CSV.
    #   see details
    # timeout_: integer. the amount of time to wait for a response from the server.
    # timeout_action: character. either "skip" to move on to the next chunk,
    #   or "fail" to raise an error.
    # warn: logical. if TRUE and more than 1 million rows are queried, this function will
    #   ask for confirmation before proceeding.
    # verbose: logical. if TRUE, you'll receive more information.
    # debug_: logical; only for interactive use. If TRUE, the debugger will be
    #   entered before the first request is executed.

    #DETAILS
    #Some envirofacts tables generate errors when returning one format or the
    #   other (JSON or CSV). The output of this function will be a
    #   data.frame (tibble) in either case. This parameter is just a way of fiddling with
    #   an imperfect data retrieval system.

    #RETURN VALUE
    #an all-character tibble of evirofacts results

    if(! rtn_fmt %in% c('csv', 'json')){
        stop('rtn_fmt must be "csv" or "json"')
    }

    if(! timeout_action %in% c('skip', 'fail')){
        stop('timeout_action must be "skip" or "fail"')
    }

    nrows = query_ef_rows(table_name = table_name,
                          column_name = column_name,
                          operator = operator,
                          column_value = column_value)

    if(warn && nrows > 1e6){
        resp = get_response_1char(msg = paste('This table has',
                                              nrows,
                                              'rows. still load into memory? (y/n) >'),
                                  c('y', 'n'))
        if(resp == 'n'){
            message('aborted query')
            rtn_abrt = 'user aborted'
            class(rtn_abrt) = 'ef_err'
            return(rtn_abrt)
        }
    } else {
        if(verbose) print(paste('This table has', nrows, 'row(s).'))
    }

    if(nrows == 0){
        if(verbose) message('Returning empty tibble')
        return(tibble())
    }

    if(is.null(custom_chunks)){
        chunksets = get_chunksets(nrows=nrows,
                                  maxrows=chunk_size)
    } else {
        chunksets <- custom_chunks
    }

    full_table = tibble()
    task_start = proc.time()
    if(specify_rows){
        for(i in seq_along(chunksets)){

            if(verbose) print(paste0('Retrieving chunk ', i, ' of ', length(chunksets),
                                    '; (', chunksets[i], ')'))

            chnk = get_ef_tablechunk(table_name=table_name,
                                     column_name=column_name,
                                     operator=operator,
                                     column_value=column_value,
                                     rows=chunksets[i],
                                     rtn_fmt=rtn_fmt,
                                     timeout_=timeout_,
                                     timeout_action=timeout_action,
                                     debug_=debug_)

            full_table = bind_rows(full_table, chnk)
        }
    } else {
        if(nrows >= 1e5) stop('envirofacts does not allow queries of more than 100,000 rows.')

        if(verbose) print(paste('attempting to retrieve all rows (specify_rows is FALSE).'))

        full_table = get_ef_tablechunk(table_name=table_name,
                                       column_name=column_name,
                                       operator=operator,
                                       column_value=column_value,
                                       rows=NULL,
                                       rtn_fmt=rtn_fmt)
    }

    task_time = unname(round((proc.time() - task_start)[3] / 60, 2))
    if(verbose) print(paste0('Got table ', table_name, ' (', nrows, ' rows) in ', task_time, ' minutes'))

    return(full_table)
}

# table_name = c('ICIS_LIMIT'); rows = '1:10'
# table_names = c('tri_facility', 'tri_reporting_form', 'tri_chem_info'); rows = '1:10'
# column_name = 'state_abbr'; operator = '='; column_value = 'VA'
# return_count = FALSE
# table_name = c('ICIS_LIMIT'); rows = '1:10'

# query_ef_rows('XREF_AIR_FAC_INT_PROG_SUBPART')
# xx = query_ef_table('ref_sic')

query_envirofacts = function(table_names,
                             filter_column=NULL,
                             filter_operator=NULL,
                             filter_value=NULL,
                             join_column,
                             save_intermediates=FALSE,
                             warn=TRUE){

    # table_names: char vector of envirofacts table names
    # filter_column: char: optional; use to filter by column value (see filter_operator). must be shared among all tables in table_names.
    # filter_operator: =, !=, <, >, BEGINNING, CONTAINING; if filtering, the filter expression's filter_operator (with filter_value)
    # filter_value: if filtering, the value on the RHS of the filtering expression
    # join_column: the column by which to join tables in table_names. must be shared by all.
    # save_intermediates: logical. save each table's results to global env, in case something goes wrong?
    #   DEPRECATED? (not yet): using arrow to manipulate files on disk, so saving intermediates to disk now required.

    #combines results from several envirofacts tables.

    combined_results = tibble()
    for(i in seq_along(table_names)){

        tname = table_names[i]

        print(paste('Working on', tname))

        table_result = query_ef_table(tname,
                                      column_name=filter_column,
                                      operator=filter_operator,
                                      column_value=filter_value,
                                      warn=warn)

        if(inherits(table_result, 'ef_err')) return()

        if(save_intermediates){
            print(paste('saving table', tname, 'to global env'))
            assign(tname, table_result, pos=.GlobalEnv)
        }

        if(nrow(table_result)){

            shared_cols = intersect(colnames(combined_result),
                                    colnames(table_result))
            shared_cols = shared_cols[shared_cols != join_column]

            if(length(shared_cols)){
                message(paste('dropping redundant columns from :',
                              paste(shared_cols, collapse = ', ')))
            }

            table_result = select(table_result, -any_of(shared_cols))
            combined_results = full_join(combined_result, table_result,
                                         by = toupper(join_column))
        } else {

            if(! is.null(filter_column)){
                warning(paste('no rows in', tname, 'for', filter_column,
                              operator, filter_value))
            } else {
                warning(paste('no rows in', tname))
            }
        }

    }

    return(combined_results)
}

dms_to_decdeg = function(x){

    #x: an integer or character vector of latlongs in dms format, e.g. "123456" or 1234567
    #                                                                   DDMMSS     DDDMMSS

    decdeg = c()
    for(i in seq_along(x)){

        xx = x[i]

        if(is.na(xx)){
            decdeg[i] = NA
            next
        }

        if(! nchar(xx) %in% 6:7){
            warning(paste(nchar(xx), 'characters in x. need 6 (or 7 for some longitudes)'))
            decdeg[i] = NA
        }

        deginc = if(nchar(xx) == 7) 1 else 0

        degs = as.numeric(substr(xx, 1, 2 + deginc))
        mins = as.numeric(substr(xx, 3 + deginc, 4 + deginc))
        secs = as.numeric(substr(xx, 5 + deginc, 6 + deginc))

        decdeg[i] = degs + mins / 60 + secs / 3600
    }

    return(decdeg)
}

clean_county_names = function(x){

    x = toupper(x)
    x = gsub('[\\. \\-]', '', x)
    x = sub('PARISH$', '', x)
    x = sub('THEBAPTIST$', '', x)
    x = sub('^SAINT', 'ST', x)

    return(x)
}

ej_heatmap_kde = function(d, center, scale, bw=0.005, res=1000, addpoints=FALSE, fileout){

    klonmin = min(d$lon) - .02
    klonmax = max(d$lon) + .02
    klatmin = min(d$lat) - .02
    klatmax = max(d$lat) + .02

    kde = bkde2D(as.matrix(d[, c('lon', 'lat')]),
                 bandwidth = c(bw, bw),
                 gridsize = c(res, res),
                 # range.x = list(c(-94.25, -94), c(29.8, 30.1)))
                 range.x = list(c(klonmin, klonmax), c(klatmin, klatmax)),
                 truncate = FALSE)


    # Create Raster from Kernel Density output
    KernelDensityRaster = raster(list(x=kde$x1, y=kde$x2, z=kde$fhat))
    # plot(KernelDensityRaster)

    # #create pal function for coloring the raster
    # palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values)
    #
    # ## Leaflet map with raster
    # leaflet() %>% addTiles() %>%
    #     addRasterImage(KernelDensityRaster,
    #                    colors = palRaster,
    #                    opacity = .8) %>%
    #     addLegend(pal = palRaster,
    #               values = KernelDensityRaster@data@values,
    #               title = "Kernel Density of Points")

    #set low density cells as NA so we can make them transparent with the colorNumeric function
    KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 1)] = NA

    # #create pal function for coloring the raster
    # palRaster = colorNumeric("Spectral",
    #                          domain = KernelDensityRaster@data@values,
    #                          na.color = "transparent")

    palRaster = colorBin("Spectral",
                         bins = 7,
                         domain = KernelDensityRaster@data@values,
                         na.color = "transparent")

    ## Redraw the map
    mapout = leaflet() %>%
        # addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = center[1],
                lat = center[2],
                zoom = scale) %>%
        addRasterImage(KernelDensityRaster,
                       colors = palRaster,
                       opacity = .8) %>%
        addLegend(pal = palRaster,
                  values = KernelDensityRaster@data@values,
                  title = "Kernel Density of Points")

    if(addpoints){

        mapout = mapout %>%
            addCircles(lng = d$lon, lat = d$lat,
                       radius = .5, opacity = .2, col = "blue")
    }

    mapshot(mapout, file = fileout)
}

ej_heatmap = function(d, center, scale, res, latrange, lonrange, addpoints=FALSE, fileout){

    r = rast(resolution=res, xmin = lonrange[1], xmax = lonrange[2],
             ymin = latrange[1], ymax = latrange[2], crs='EPSG:4326')

    dvect = vect(d, geom=c('lon', 'lat'), crs = 'EPSG:4326')
    drast = rasterize(dvect, r, field='load_kg', fun=sum)

    # palRaster = colorBin('Spectral',
                         # bins = 7,
                         # reverse = TRUE,
    # palRaster = colorNumeric('Spectral',
    # palRaster = colorNumeric('magma',
    # palRaster = colorNumeric(magma(n=50, begin = 0.2, end = 0.9),
    # palRaster = colorNumeric(magma(n=50, begin = 0.2, end = 0.9),
    palRaster = colorNumeric(colorRampPalette(c('blue', 'red'))(50),
                             domain = values(drast),
                             na.color = 'transparent',
                             reverse = FALSE)

#     tag.map.title <- tags$style(HTML("
#   .leaflet-control.map-title {
#     transform: translate(-50%,20%);
#     position: fixed !important;
#     left: 50%;
#     text-align: center;
#     padding-left: 10px;
#     padding-right: 10px;
#     background: rgba(255,255,255,0.75);
#     font-weight: bold;
#     font-size: 28px;
#   }
# "))
#
#     title <- tags$div(
#         tag.map.title, HTML("Map title")
#     )

    mapout = leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = center[1],
                lat = center[2],
                zoom = scale) %>%
        addRasterImage(raster(drast),
                       colors = palRaster,
                       opacity = .6) %>%
        addLegend(pal = palRaster,
                  values = values(drast),
                  title = 'Cumulative Load (kg)')
        # addControl(title, position = "topleft", className="map-title")

    if(addpoints){

        mapout = mapout %>%
            # addCircles(lng = d$lon, lat = d$lat, opacity = .5, weight = 3,
            #            radius = 1, col = 'black', fill = FALSE)
            addCircleMarkers(lng = d$lon, lat = d$lat, opacity = 1, weight=1,
                             radius = 1, col = 'black', fill = FALSE)
    }

    if(grepl('\\.html$', fileout)){
        mapshot(mapout, url = fileout)
    } else {
        mapshot(mapout, file = fileout)
    }
}
