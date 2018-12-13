#' Find pmids that not in database
#'
#' `find_pmids_not_in_db` return the pmids that are not in the database.

#' @details
#' `find_pmids_not_in_db` will search EdgeList_date for matching pmids and 
#' return the pmids that are not in the database.
#'
#' @param conMysql connection to mysql as defined in ~/.my.cnf
#' @param pmids pmids a vector of PMIDs look-up 
#' @param tableName string with the name of the table used to store the edge list
#' @param targetName string with the name of the column used to store the Target of edge list
#' @return find_pmids_not_in_db() return the pmids that are not in the database.
#'
#' @seealso \code{\link{create_date_table}} for edge list date table structure
#'
#' @examples
#' # In this example, PMIDs 123 and 456 are in the database
#' pmids_lookup <- c(123, 456, 759, 869)
#' res <- find_pmids_not_in_db(conMysql, pmids_lookup)
#' # res will print out 759 and 869

#' @export

find_pmids_not_in_db <-function(conMysql, pmids, tableName = "EdgeList_Date", targetName = "Target") {
  # This will search if tableName exist in database
  qry <- paste0("show tables like '",tableName,"';")
  res <- dbGetQuery(conMysql, qry)
  
  # checking if table exist
  if (length(res[[1]]) == 1){
    if (length(pmids) != 0){
      print("find_pmids_not_in_db: Table was found. Running.")
      
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
      print("find_pmids_not_in_db: Input is empty. Nothing to query in database.")
      res <- NULL
    }
  }else{
    print("find_pmids_not_in_db: Table does not exist in database.")
    res <- NULL
  }
}



