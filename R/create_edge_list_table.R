#' Create edge list table if not found.
#'
#' `create_edge_list_table` create edge list table in MySQL.

#' @details
#' Default values for this table is: 
#' tableName = "EdgeList", sourceName = "SourcePMID", targetName = "Target"
#' MySQL statment:
#' CREATE TABLE Edgelist_date (
#' SourcePMID INT,
#' Target INT,
#' INDEX index_target(Target));
#' 
#' @param con_mysql connection to mysql as defined in ~/.my.cnf
#' @param tableName string with the name of the table used to store the edge list
#' @param sourceName string with the name of the column used to store the source of edge list
#' @param targetName string with the name of the column used to store the Target of edge list
#' @return create_edge_list_table() always returns a scalar numeric that specifies the number of rows
#'         affected by the statement. An error is raised when issuing a statement over a closed
#'         or invalid connection, if the syntax of the statement is invalid, or if the statement is
#'         not a non-NA string.
#'
#' @examples
#'

#' @export
create_edge_list_table <- function(con_mysql, tableName = "EdgeList", sourceName = "Source", targetName = "Target"){
  # This will search if tableName exist in database
  qry <- paste0("show tables like '",tableName,"';")
  res <- dbGetQuery(con_mysql, qry)
  
  # Create the table if it is not found by checking length of res
  if (length(res[[1]]) == 0){
    print("create_edge_list_table: No table found. Creating table now.")
    qry <- paste0("CREATE TABLE ",tableName," (
                  ",sourceName," INT,
                  ",targetName," INT,
                  INDEX index_target(",targetName,"));")
    dbExecute(con_mysql, qry)
  }else{
    print("create_edge_list_table: Table is found.")
  }
}