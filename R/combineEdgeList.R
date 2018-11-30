#' Combine edge list and database edge list.
#'
#' `combineEdgeList` insert the edge list from the results of `generateEdgeList`

#' @details
#' `combineEdgeList` must come from the result of generateEdgeList
#'
#' @param con_mysql connection to mysql as defined in ~/.my.cnf
#' @param new_edgelist edge_list results, as obtained from `generateEdgeList` (see details)
#' @param db_pmids list of pmids used to search database.
#' @param table_name string with the name of the table used to store the edge list
#' @param target_name string with the name of the column used to store the target of edge list
#' @return combineEdgeList() will return one dataframe after joining two datasets vertically.

#' @examples
#'
#' # place holder



#' @export
#' 
combineEdgeList <- 
  function(con_mysql, new_edgelist, db_pmids, table_name = "edgelist", target_name = "target"){
  
  db_pmids <- paste0("(",db_pmids,")")
  qry <- paste0("SELECT * FROM ",table_name," WHERE ",target_name," in ",db_pmids,";")
  res <- dbGetQuery(con_mysql, qry)
  
  combine <- rbind(new_edgelist, res)
}