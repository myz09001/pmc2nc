#' Retrieve edge list from database
#'
#' `get_edge_list_db` will retrieve edge list from database with the provided list of PMIDs.

#' @details
#' `get_edge_list_db` will retrieve edge list from database with the provided list of PMIDs.
#' If table is not found in database, NULL will be returned
#'
#' @param conMysql connection to mysql as defined in ~/.my.cnf
#' @param pmids list of pmids used to search database.
#' @param tableName string with the name of the table used to store the edge list
#' @param targetName string with the name of the column used to store the Target of edge list
#' @return get_edge_list_db() will return the edge list in dataframe
#' 
#' @seealso \code{\link{create_edge_list_table}} for edge list table structure
#' 
#' @examples
#' # This will look for PMIDs in 'x' from the DB and get the Source and Target
#' x <- c(21876761, 311, 29463753, 21876726)
#' res <- get_edge_list_db(conMysql, x)



get_edge_list_db <- 
  function(conMysql, pmids, tableName = "EdgeList", targetName = "Target"){
    # This will search if tableName exist in database
    qry <- paste0("show tables like '",tableName,"';")
    res <- dbGetQuery(conMysql, qry)
    
    # If the table do exist, check if pmids argument actually have values.
    if (length(res[[1]]) == 1){
      
      if (length(pmids) != 0){
      
        print("get_edge_list_db: Querying edge list from database.")
        
        # format list of pmids for mysql query
        pmids <- paste0(pmids, collapse = ",")
        #pmids <- paste0("(",pmids,")")
        
        # SELECT * FROM edgelist WHERE Target in (21876761, 311, 29463753, 21876726)
        qry <- paste0("SELECT * FROM ",tableName," WHERE ",targetName," in (",pmids,");")
        res <- dbGetQuery(conMysql, qry)
      }else{
        print("get_edge_list_db: PMID input is empty. Cannot search database.")
        res <- NULL
      }
    }else{
      print("get_edge_list_db: Table does not exist in database.")
      res <- NULL
    }
  }