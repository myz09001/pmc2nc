#' Retrieve an edge list from the provided PMIDs
#'
#' `retrieveEdgeList` insert the edge list from the results of `generateEdgeList`

#' @details
#' `edge_list` must come from the result of generateEdgeList
#'
#' @param pmids a vector of PMIDs look-up.
#' @param batchSize the batch size to use.
#' @param con_mysql connection to mysql as defined in `~/.my.cnf`.
#' @param lastupdate string with the name of the table used to store the edge list.
#' @return An edge list (data.frame) with one column for target PMIDS and one column for source PMIDS.
#' 
#' @seealso \code{\link{get_pmc_cited_in}} for obtaining edge_list citation results
#' @seealso \code{\link{generateEdgeList}} for obtaining edge_list citation results
#'
#' @examples
#'
#' # place holder

# the ultimate function that does everything
#retrieveEdgeList (pmids, batchSize = 200, con = NULL, lastUpdate = NULL){
#  1. if con is not NULL then
#  (pmids) 1. pmids1(in db and update >= last update)
#          2. pmids2(not in db and not updated table because that means it searched but not results in NCI)

#  2. get edge list from NCBI
#        res1 <- get_pmc_cited_in(pmids2)
#        e1 <- generateEgeList(res1)
#        -> add to db and update the update table if con is not NULL

#  3. If specified, get edgelist from DB
#        e2 <- get_edge_list_from_db(con_mysql, pmids)

#  4. combine_edge_list

#  5. return edge list
#}

#' @export

retrieveEdgeList <- function(pmids, batchSize = 200, con_mysql = NULL, lastupdate = NULL){
  if(!(con_mysql = NULL)){
    
    if(is(con_mysql, "MariaDBConnection")){
      print("true maria")
      
      edgelist_from_db <- get_edge_list_db(con, pmids)
    }else{
      print("You 'con' argument is not a MariaDBConnection")
      # figure how to stop retrieveEdgeList() early
    }
    
    # combine comes here
    
  }else{
    res_from_NCBI <- get_pmc_cited_in(pmids, batchSize)
    e1 <- generateEdgeList1(res_from_NCBI)
  }
  
  
  
  
  
}