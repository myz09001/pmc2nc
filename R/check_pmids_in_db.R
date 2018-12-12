#' Check Edge List Database
#'
#' `check_pmids_in_db` check if PMIDs have been searched and saved in database already.

#' @details
#' `check_pmids_in_db` will search EdgeList_date for matching pmids. 
#'
#' @param conMysql connection to mysql as defined in ~/.my.cnf
#' @param pmids pmids a vector of PMIDs look-up 
#' @param tableName string with the name of the table used to store the edge list
#' @param targetName string with the name of the column used to store the Target of edge list
#' @return check_pmids_in_db() return the pmids that are not in the database.
#'
#' @seealso \code{\link{create_date_table}} for edge list date table structure
#'
#' @examples
#' # In this example, PMIDs 123 and 456 are in the database
#' pmids_lookup <- c(123, 456, 759, 869)
#' res <- check_pmids_in_db(conMysql, pmids_lookup)
#' # res will print out 759 and 869

#' @export

check_pmids_in_db <-function(conMysql, pmids, tableName = "EdgeList_Date", targetName = "Target") {
  # This will search if tableName exist in database
  qry <- paste0("show tables like '",tableName,"';")
  res <- dbGetQuery(conMysql, qry)
  
  # If the table do exist, run check table for pmids else do nothing
  if (length(res[[1]]) == 1){
    print("Table was found. Running check_pmids_in_db()")
    
    # format pmids to use for query
    qry_pmids <- paste0( pmids, collapse = ", ")
  
    # select statement to find pmids already in DB
    qry <- paste0("SELECT ",targetName," FROM ",tableName," WHERE ",targetName," in (",qry_pmids,");")
    res <- dbGetQuery(conMysql, qry)
  
    # find the matching pmids in the DB
    match_pmids <- pmids %in% res[[targetName]]
    
    # only keep the pmids that are not in the database already
    new_pmids <- pmids[!match_pmids]
  }else{
    print("Table does not exist in database.")
    res <- NULL
  }
}



