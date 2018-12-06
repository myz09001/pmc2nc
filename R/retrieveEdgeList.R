#' Retrieve an edge list from the provided PMIDs
#'
#' `retrieveEdgeList` insert the edge list from the results of `generateEdgeList`

#' @details
#' `edge_list` must come from the result of generateEdgeList
#'
#' @param pmids a vector of PMIDs look-up.
#' @param batchSize the batch size to use.
#' @param conMysql connection to mysql as defined in `~/.my.cnf`.
#' @param lastUpdate string of a date used to find old PMIDs stored in database in YYYY-MM-DD format.
#' @param saveInDB to choice between storing edge list in database or not. 1 = save in DB, 2 = do not save.
#' @return An edge list (data.frame) with one column for target PMIDS and one column for source PMIDS.
#' 
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
#        e2 <- get_edge_list_from_db(conMysql, pmids)

#  4. combine_edge_list

#  5. return edge list
#

#' @export

retrieveEdgeList <- function(pmids, batchSize = 200, conMysql = NULL, lastUpdate = NULL, saveInDB = 1){
  if(!(conMysql = NULL)){
    
    if(is(conMysql, "MariaDBConnection")){
      print("true maria")
      
      # only keep the pmids that are not in the database already
      pmids_not_in_db <- check_pmids_in_db(pmids)
      
      # find the matching pmids that are not in database
      match_pmids <- pmids %in% pmids_not_in_db
      
      # only keep the pmids that is in database
      pmids_in_db <- pmids[!match_pmids]
      
      
      edgelist_from_db <- get_edge_list_db(conMysql, pmids)
      
      # find the matching pmids in the DB
      #match_pmids <- pmids %in% res[[targetName]]
      
      # only keep the pmids that are not in the database already
      #new_pmids <- pmids[!match_pmids]
      
      
    }else{
      print("You 'con' argument is not a MariaDBConnection")
      # figure how to stop retrieveEdgeList() early
    }
    
    # combine comes here
    
  }else{
    res <- get_pmc_cited_in(pmids, batchSize)
    e1 <- generateEdgeList1(res)
  }
  
  
  
  
  
}