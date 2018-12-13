#' Insert edge lists to mysql database.
#'
#' `insertEdgeList` insert the edge list from the results of `generateEdgeList`

#' @details
#' `edgeList` must come from the result of generateEdgeList. The edge list will be stored in to
#' table called Edgelist by default. This table have SourcePMID and Target column. It will also store
#' the date when the target PMID was inserted to the database in a table called EdgeList_date.
#'
#' @param conMysql connection to mysql as defined in ~/.my.cnf
#' @param edgeList edgeList results, as obtained from `generateEdgeList` (see details)
#' @param tableName string with the name of the table used to store the edge list
#' @param sourceName string with the name of the column used to store the source of edge list
#' @param targetName string with the name of the column used to store the Target of edge list
#' @param dateTableName string with the name of the table used to store the dates for pmids
#' @return insertEdgeList() always returns a scalar numeric that specifies the number of rows
#'         affected by the statement. An error is raised when issuing a statement over a closed
#'         or invalid connection, if the syntax of the statement is invalid, or if the statement is
#'         not a non-NA string.
#' @seealso \code{\link{get_pmc_cited_in}} for obtaining edgeList citation results
#' @seealso \code{\link{generateEdgeList}} for obtaining edgeList citation results
#' @seealso \code{\link{create_edge_list_table}} for edge list table structure
#' @seealso \code{\link{create_date_table}} for edge list date table structure
#'
#' @examples
#'
#' # generate an edge list for a single article and then insert the data to the table called "edgelist"
#' res1 <- get_pmc_cited_in(21876761)
#' e2 <- generateEdgeList(res1)
#' insertEdgeList(conMysql, e2)


#' @export

insertEdgeList <-
  function(conMysql, edgeList, tableName = "EdgeList", sourceName = "Source", targetName = "Target", dateTableName = "EdgeList_date"){
    if (length(edgeList) != 0){
      print("insertEdgeList: Inserting edge list into database now.")
      # Check if table exist in database and create table if it does not
      create_edge_list_table(conMysql, tableName, sourceName, targetName)
      
      # Check if date table exist in database and create date table if it does not  
      create_date_table(conMysql, dateTableName, targetName)
      
      # insert dates for edgeList$Target in 'EdgeList_date' table
      mysql_date_qry_statement(conMysql, unique(edgeList$Target), dateTableName, targetName)
      
      # To avoid hitting any memory limits, only process 1 million rows at a time
      rowcount <-nrow(edgeList)
      if( rowcount > 1000000){
        n <- ceiling(rowcount / 1000000)
        edgeList <- split(edgeList, 1:n)
        
        # this will insert in batches
        for(x in edgeList){
          mysql_qry_statement(conMysql, x, tableName, targetName)
        }
      }else{
        # this will insert in all in one go
        mysql_qry_statement(conMysql, edgeList, tableName, targetName)
      }
    }else{
      print("insertEdgeList: Input is empty. Nothing to insert to database.")
      res <- NULL
    }
}

mysql_qry_statement <- function(conMysql, edgeList, tableName, targetName){
  # delete old edge list so there is no duplicates
  target_delete <- unique(edgeList$Target)
  for(x in target_delete){
    delete_paste <- paste0("DELETE FROM ",tableName," where ",targetName," = '",x,"';")
    dbExecute(conMysql, delete_paste)
  }
  
  # concatenate the two columns of edgeList so I can insert the whole thing in one insert statement
  edge_list_paste <- paste0("(" , edgeList$Source , "," , edgeList$Target, ")", collapse = ",")

  # Insert statement for MySQL into tableName
  qry <- paste0("INSERT INTO ",tableName," Values ",edge_list_paste,";")

  # execute the insert statement
  dbExecute(conMysql, qry)
}

mysql_date_qry_statement <- function(conMysql, edgeList, tableName, targetName){
  # delete old dates so there is no duplicates
  for(x in edgeList){
    delete_paste <- paste0("DELETE FROM ",tableName," where ",targetName," = '",x,"';")
    dbExecute(conMysql, delete_paste)
  }
  
  # concatenate the two columns of edgeList so I can insert the whole thing in one insert statement
  date_paste <- paste0("(" , edgeList , ",\'" , Sys.Date(), "\')", collapse = ",")

  # Insert statement for MySQL into tableName
  qry <- paste0("INSERT INTO ",tableName," Values ",date_paste,";")

  # execute the insert statement
  dbExecute(conMysql, qry)
}
