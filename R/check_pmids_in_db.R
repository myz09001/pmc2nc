#' Check Edge List Database
#'
#' `check_pmids_in_db` check if PMIDs have been searched and saved in database already. Only return PMIDs that are new.

#' @details
#' `pmids` comes from user on shiny app
#'
#' @param conMysql connection to mysql as defined in ~/.my.cnf
#' @param edgeList edgeList results, as obtained from `generateEdgeList` (see details)
#' @param tableName string with the name of the table used to store the edge list
#' @param targetName string with the name of the column used to store the target of edge list
#' @return check_pmids_in_db() return the pmids that are not already in the database.
#'

#' @export

check_pmids_in_db <-function(conMysql, pmids, tableName = "EdgeList_Date", targetName = "target") {
  # This will search if tableName exist in database
  qry <- paste0("show tables like '",tableName,"';")
  res <- dbGetQuery(conMysql, qry)
  
  # If the table do exist, run check table for pmids else do nothing
  if (length(res[[1]]) == 1){
    # format pmids to use for query
    qry_pmids <- paste0('\'', pmids,  '\'', collapse = " or ")
  
    # select statement to find pmids already in DB
    qry <- paste0("SELECT DISTINCT ",targetName," FROM ",tableName," WHERE ",targetName," = ",qry_pmids,";")
    res <- dbGetQuery(conMysql, qry)
  
    # find the matching pmids in the DB
    match_pmids <- pmids %in% res[[targetName]]
  
    # only keep the pmids that are not in the database already
    new_pmids <- pmids[!match_pmids]
  }else{
    print("Table does not exist in database.")
    pmids
  }
}



