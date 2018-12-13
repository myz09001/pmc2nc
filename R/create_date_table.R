#' Create edge list date table if not found.
#'
#' `create_date_table` create edge list date table in MySQL

#' @details
#' Default values for this table is: 
#' tableName = "EdgeList_date", targetName = "Target", dateName = "DatePMID"
#' MySQL statment:
#' CREATE TABLE Edgelist_date (
#' Target INT,
#' DatePMID date,
#' INDEX index_target(Target));
#' 
#' @param conMysql connection to mysql as defined in ~/.my.cnf
#' @param tableName string with the name of the table used to store the dates for pmids
#' @param targetName string with the name of the column used to store the Target of edge list
#' @param dateName string with the name of the column used to store the date of edge list
#' @return create_date_table() always returns a scalar numeric that specifies the number of rows
#'         affected by the statement. An error is raised when issuing a statement over a closed
#'         or invalid connection, if the syntax of the statement is invalid, or if the statement is
#'         not a non-NA string.

#' @export
create_date_table <- function(conMysql, tableName = "EdgeList_date", targetName = "Target", dateName = "DatePMID"){
  # This will search if tableName exist in database
  qry <- paste0("show tables like '",tableName,"';")
  res <- dbGetQuery(conMysql, qry)
  
  # Create the table if it is not found by checking length of res
  if (length(res[[1]]) == 0){
    print("create_date_table: No table found. Creating date table now.")
    qry <- paste0("CREATE TABLE ",tableName," (
                  ",targetName," INT,
                  ",dateName," date,
                  INDEX index_target(",targetName,"));")
    dbExecute(conMysql, qry)
  }else{
    print("create_date_table: Date table is found.")
  }
}
