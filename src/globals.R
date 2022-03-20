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
                             rows=NULL){

    # table_name: char string; the name of an envirofacts table
    # column_name: char: optional; use to filter by column value (see operator)
    # operator: =, !=, <, >, BEGINNING, CONTAINING; if filtering, the filter expression's operator (with column_value)
    # column_value: if filtering, the value on the RHS of the filtering expression
    # rows: 'x:y'; max of 100,000

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

    # query = 'https://data.epa.gov/efservice/ICIS_LIMIT/REF_POLLUTANT/SRS_ID/439216/rows/1:5/JSON'
    # query='https://data.epa.gov/efservice/ICIS_LIMIT/SRS_ID/=/5520/csv' ##***
    query = glue('{qb}{tn}{cn}{op}{cv}{tnx}{ro}/csv',
                 qb=query_base,
                 tn=paste(table_name, collapse='/'),
                 cn=ifelse(is.null(column_name), '', paste0('/', column_name)),
                 op=ifelse(is.null(operator), '', paste0('/', operator)),
                 cv=ifelse(is.null(column_value), '', paste0('/', column_value)),
                 tnx=table_names_ext,
                 ro=ifelse(! is.null(rows), paste0('/rows/', rows), ''))

    r = httr::GET(query)
    d = httr::content(r, as='text', encoding='UTF-8')
    # d = jsonlite::fromJSON(d)

    d = read_csv(d, col_types = cols(.default = 'c')) %>%
        select(-starts_with('...')) %>%
        rename_with(function(x) str_match(x, '\\.([^\\.]+)$')[, 2])

    #needed if parsing json
    # if(inherits(d, 'list') && ! inherits(d, 'data.frame')){
    #     d = as_tibble(d)
    # }

    return(d)
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

get_chunksets = function(nrows, maxrows){

    # nrows: the number of rows in an envirofacts table. probably queried via query_ef_rows()
    # maxrows: the maximum number of rows that can be returned via the envirofacts REST-API

    nchunks = nrows %/% maxrows
    rem = nrows %% maxrows
    if(rem > 0) nchunks = nchunks + 1

    chunk_starts = seq(1, nrows, maxrows)
    chunk_ends = 1:nchunks * maxrows
    if(rem != 0){
        chunk_ends[nchunks] = chunk_starts[nchunks] + rem - 1
    }

    chunk_sets = paste(chunk_starts, chunk_ends,
                       sep=':')

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

    query = glue('{qb}{tn}{cn}{op}{cv}/count/json',
                 qb=query_base,
                 tn=paste(table_name, collapse='/'),
                 cn=ifelse(is.null(column_name), '', paste0('/', column_name)),
                 op=ifelse(is.null(operator), '', paste0('/', operator)),
                 cv=ifelse(is.null(column_value), '', paste0('/', column_value)))

    r = httr::GET(query)
    d = httr::content(r, as='text', encoding='UTF-8')
    d = jsonlite::fromJSON(d)[[1]]

    return(d)
}

query_ef_table = function(table_name,
                          column_name=NULL,
                          operator=NULL,
                          column_value=NULL,
                          warn=TRUE){

    # table_name: char string; the name of an envirofacts table
    # column_name: char: optional; use to filter by column value (see operator)
    # operator: =, !=, <, >, BEGINNING, CONTAINING; if filtering, the filter expression's operator (with column_value)
    # column_value: if filtering, the value on the RHS of the filtering expression

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
        print(paste('This table has', nrows, 'row(s).'))
    }

    if(nrows == 0){
        message('Returning empty tibble')
        return(tibble())
    }

    chunksets = get_chunksets(nrows=nrows,
                              maxrows=1e5)

    full_table = tibble()
    task_start = proc.time()
    for(i in seq_along(chunksets)){

        print(paste('Retrieving chunk', i, 'of', length(chunksets)))

        chnk = get_ef_tablechunk(table_name=table_name,
                                 column_name=column_name,
                                 operator=operator,
                                 column_value=column_value,
                                 rows=chunksets[i])

        full_table = bind_rows(full_table, chnk)
    }

    task_time = unname(round((proc.time() - task_start)[3] / 60, 2))
    print(paste0('Got table ', table_name, ' (', nrows, ' rows) in ', task_time, ' minutes'))

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

