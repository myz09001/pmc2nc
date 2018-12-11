#' Insert edge lists to mysql database.
#'
#' `insertEdgeList` insert the edge list from the results of `generateEdgeList`

#' @details
#' `edge_list` must come from the result of generateEdgeList
#'
#' @param con_mysql connection to mysql as defined in ~/.my.cnf
#' @param edge_list edge_list results, as obtained from `generateEdgeList` (see details)
#' @param table_name string with the name of the table used to store the edge list
#' @param source_name string with the name of the column used to store the source of edge list
#' @param target_name string with the name of the column used to store the Target of edge list
#' @param date_table_name string with the name of the table used to store the dates for pmids
#' @return insertEdgeList() always returns a scalar numeric that specifies the number of rows
#'         affected by the statement. An error is raised when issuing a statement over a closed
#'         or invalid connection, if the syntax of the statement is invalid, or if the statement is
#'         not a non-NA string.
#' @seealso \code{\link{get_pmc_cited_in}} for obtaining edge_list citation results
#' @seealso \code{\link{generateEdgeList}} for obtaining edge_list citation results
#'
#' @examples
#'
#' # generate an edge list for a single article and then insert the data to the table called "edgelist"
#' res1 <- get_pmc_cited_in(21876761)
#' e2 <- generateEdgeList(res1)
#' insertEdgeList(con_mysql, e2)


#' @export

insertEdgeList <-
  function(con_mysql, edge_list, table_name = "EdgeList", source_name = "source", target_name = "Target", date_table_name = "EdgeList_date"){
  # Check if table exist in database and create table if it does not
    create_edge_list_table(con_mysql, table_name, source_name, target_name)
  
  # Check if date table exist in database and create date table if it does not  
    create_date_table(con_mysql, date_table_name, target_name)

  # insert dates for edge_list$Target in 'EdgeList_date' table
  mysql_date_qry_statement(con_mysql, unique(edge_list$Target), date_table_name, target_name)
  
  # To avoid hitting any memory limits, only process 1 million rows at a time
  rowcount <-nrow(edge_list)
  if( rowcount > 1000000){
    n <- ceiling(rowcount / 1000000)
    edge_list <- split(edge_list, 1:n)
    
    # this will insert in batches
    for(x in edge_list){
      mysql_qry_statement(con_mysql, x, table_name, target_name)
    }
  }else{
    # this will insert in all in one go
    mysql_qry_statement(con_mysql, edge_list, table_name, target_name)
  }
}

mysql_qry_statement <- function(con_mysql, edge_list, table_name, target_name){
  # delete old edge list so there is no duplicates
  target_delete <- unique(edge_list$Target)
  for(x in target_delete){
    delete_paste <- paste0("DELETE FROM ",table_name," where ",target_name," = '",x,"';")
    dbExecute(con_mysql, delete_paste)
  }
  
  # concatenate the two columns of edge_list so I can insert the whole thing in one insert statement
  edge_list_paste <- paste0("(" , edge_list$Source , "," , edge_list$Target, ")", collapse = ",")

  # Insert statement for MySQL into table_name
  qry <- paste0("INSERT INTO ",table_name," Values ",edge_list_paste,";")

  # execute the insert statement
  dbExecute(con_mysql, qry)
}

mysql_date_qry_statement <- function(con_mysql, edge_list, table_name, target_name){
  # delete old dates so there is no duplicates
  for(x in edge_list){
    delete_paste <- paste0("DELETE FROM ",table_name," where ",target_name," = '",x,"';")
    dbExecute(con_mysql, delete_paste)
  }
  
  # concatenate the two columns of edge_list so I can insert the whole thing in one insert statement
  date_paste <- paste0("(" , edge_list , ",\'" , Sys.Date(), "\')", collapse = ",")

  # Insert statement for MySQL into table_name
  qry <- paste0("INSERT INTO ",table_name," Values ",date_paste,";")

  # execute the insert statement
  dbExecute(con_mysql, qry)
}
