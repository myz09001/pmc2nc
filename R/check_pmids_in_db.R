#' Check Edge List Database
#'
#' `check_pmids_in_db` check if PMIDs have been searched and saved in database already. Only return PMIDs that are new.

#' @details
#' `pmids` comes from user on shiny app
#'
#' @param con_mysql connection to mysql as defined in ~/.my.cnf
#' @param edge_list edge_list results, as obtained from `generateEdgeList` (see details)
#' @param table_name string with the name of the table used to store the edge list
#' @param target_name string with the name of the column used to store the target of edge list
#' @return check_pmids_in_db() return the pmids that are not already in the database.
#'

#' @export

check_pmids_in_db <-function(con_mysql, pmids, table_name = "EdgeList", target_name = "Target_PMID") {
  # This will search if table_name exist in database
  qry <- paste0("show tables like '",table_name,"';")
  res <- dbGetQuery(con_mysql, qry)
  
  # If the table do exist, run check table for pmids else do nothing
  if (length(res[[1]]) == 1){
    # format pmids to use for query
    qry_pmids <- paste0('\'', pmids,  '\'', collapse = " or ")
  
    # select statement to find pmids already in DB
    qry <- paste0("SELECT DISTINCT ",target_name," FROM ",table_name," WHERE ",target_name," = ",qry_pmids,";")
    res <- dbGetQuery(con_mysql, qry)
  
    # find the matching pmids in the DB
    match_pmids <- pmids %in% res[[target_name]]
  
    # only keep the pmids that are not in the database already
    new_pmids <- pmids[!match_pmids]
  }else{
    pmids
  }
}



