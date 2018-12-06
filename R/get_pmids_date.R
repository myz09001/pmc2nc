#' Query an pmids date from DB
#'
#' `get_pmids_date` insert the edge list from the results of `generateEdgeList`

#' @details
#' `get_pmids_date` must come from the result of generateEdgeList
#'
#' @param conMysql connection to mysql as defined in `~/.my.cnf`.
#' @param lastUpdate string of the date used for comparison.
#' @param tableName string with the name of the table used to store pmids and dates.
#' @param dateName string with the name of the column used to store the dates.
#' @param mysqlOperator string for comparison operators '= , >, >=, <, <='. 
#' @return An (data.frame) with one column for PMIDS and one column for date that pmid was saved to database.
#' 
#'
#' @examples
#' 
#' 
get_pmids_date <- function(conMysql, lastUpdate, tableName = "edgelist_date", dateName = "DatePMID", mysqlOperator = "<"){
  lastUpdate <- isDate(lastUpdate)
  if(!is.na(lastUpdate)){
    qry <- paste0("SELECT * FROM ",tableName," WHERE ",dateName," " ,mysqlOperator," '",lastUpdate,"';")
    res <- dbGetQuery(conMysql, qry)
  }else{
    print("format of last update date is incorrect. Has to be YYYY-MM-DD.")
    res <- NULL
  }
}