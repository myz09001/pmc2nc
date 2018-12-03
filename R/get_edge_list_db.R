#' Retrieve edge list from database
#'
#' `get_edge_list_db` will retrieve edge list from database with the provided list of PMIDs

#' @details
#' `combineEdgeList` must come from the result of generateEdgeList
#'
#' @param con_mysql connection to mysql as defined in ~/.my.cnf
#' @param pmids list of pmids used to search database.
#' @param table_name string with the name of the table used to store the edge list
#' @param target_name string with the name of the column used to store the target of edge list
#' @return get_edge_list_db() will return the edge list in dataframe

#' @examples
#' x <- c(21876761, 311, 29463753, 21876726)
#' get_edge_list_db(con_mysql, x)


get_edge_list_db <- 
  function(con_mysql, pmids, table_name = "edgelist", target_name = "target"){
    # format list of pmids for mysql query
    pmids <- paste0(pmids, collapse = ",")
    pmids <- paste0("(",pmids,")")
    
    # SELECT * FROM edgelist WHERE target in (21876761, 311, 29463753, 21876726)
    qry <- paste0("SELECT * FROM ",table_name," WHERE ",target_name," in ",pmids,";")
    res <- dbGetQuery(con_mysql, qry)
  }