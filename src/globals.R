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

query_envirofacts_chunk = function(table_names,
                                   column_name=NULL,
                                   operator=NULL,
                                   column_value=NULL,
                                   rows=NULL,
                                   return_count=FALSE){

    # table_names: char vect; up to 3
    # operator: =, !=, <, >, BEGINNING, CONTAINING
    # rows: 'x:y'; max of 100,000
    # return_count: if true, returns numeric count

    query_base = 'https://data.epa.gov/efservice/'

    # query = 'https://data.epa.gov/efservice/ICIS_LIMIT/REF_POLLUTANT/SRS_ID/439216/rows/1:5/JSON'
    query = glue('{qb}{tn}{cn}{op}{cv}{ro}{ct}',
                 qb=query_base,
                 tn=paste(table_names, collapse='/'),
                 cn=ifelse(is.null(column_name), '', paste0('/', column_name)),
                 op=ifelse(is.null(operator), '', paste0('/', operator)),
                 cv=ifelse(is.null(column_value), '', paste0('/', column_value)),
                 ro=ifelse(is.null(rows), '', paste0('/', rows)),
                 ct=ifelse(return_count, '/count', ''))

    r = httr::GET(query)
    d = httr::content(r, as='text', encoding='UTF-8')
    d = jsonlite::fromJSON(d)

    return(d)
}

query_envirofacts = function(table_names,
                             column_name=NULL,
                             operator=NULL,
                             column_value=NULL,
                             rows=NULL,
                             # output_format='CSV',
                             return_count=FALSE){

    length(table_names) %% 3

}

