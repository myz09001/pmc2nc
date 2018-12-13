#' Query an pmids date from DB
#'
#' `get_pmids_date` query edgelist_date table for PMIDs that match.

#' @details
#' `get_pmids_date` query edgelist_date table for PMIDs that match the comparison of the lastUpdate
#'  input and the date stored in the database.
#'
#' @param conMysql connection to mysql as defined in `~/.my.cnf`.
#' @param lastUpdate string of the date used for comparison.
#' @param tableName string with the name of the table used to store pmids and dates.
#' @param dateName string with the name of the column used to store the dates.
#' @param mysqlOperator string for comparison operators '= , >, >=, <, <='. 
#' @return An (data.frame) with one column for PMIDS and one column for date that pmid was saved to database.
#' 
#' @examples
#' 
#' # This will return a list of PMIDs if there is a match
#' date <- "2001-1-9"
#' res <- get_pmids_date(conMysql, date)
#'
#' # This will return NULL
#' date <- "209-01-9999"
#' res <- get_pmids_date(conMysql, date)
#' 
#' @export
get_pmids_date <- function(conMysql, lastUpdate, tableName = "edgelist_date", dateName = "DatePMID", mysqlOperator = "<"){
  lastUpdate <- isDate(lastUpdate)
  if(!is.na(lastUpdate)){
    print("get_pmids_date: Querying edge list insertion date.")
    qry <- paste0("SELECT * FROM ",tableName," WHERE ",dateName," " ,mysqlOperator," '",lastUpdate,"';")
    res <- dbGetQuery(conMysql, qry)
  }else{
    print("get_pmids_date: Format of last update date is incorrect. Has to be YYYY-MM-DD.")
    res <- NULL
  }
}