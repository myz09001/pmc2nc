#' Retrieve an edge list from the provided PMIDs
#'
#' `retrieveEdgeList` Retrieve an edge list from the provided PMIDs

#' @details
#'
#' @param pmids a vector of PMIDs look-up.
#' @param batchSize the batch size to use.
#' @param conMysql connection to mysql as defined in `~/.my.cnf`.
#' @param lastUpdate string of a date used to find old PMIDs stored in database in YYYY-MM-DD format.
#' @return An edge list (data.frame) with one column for target PMIDS and one column for source PMIDS.
#' 
#' 
#' @seealso \code{\link{get_pmc_cited_in}} for obtaining edge_list citation results
#' @seealso \code{\link{generateEdgeList}} for obtaining edge_list citation results
#'
#' @examples
#'

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

retrieveEdgeList <- function(pmids, batchSize = 200, conMysql = NULL, lastUpdate = NULL){
  if(!is.null(conMysql)){
    
    if(is(conMysql, "MariaDBConnection")){
      if(!is.integer(lastUpdate)){
        # Got conMysql, lastUpdate = NULL
        
        # only keep the pmids that are not in the database already
        pmids_not_in_db <- check_pmids_in_db(conMysql, pmids)
        elink_not_in_db <- get_pmc_cited_in(pmids_not_in_db)
        edgelist_not_from_db <- generateEdgeList(elink_not_in_db)
        
        # find the matching pmids that are not in database
        match_pmids <- pmids %in% pmids_not_in_db
        # only keep the pmids that is in database
        pmids_in_db <- pmids[!match_pmids]
        #################Broken here when there is NO database yet#############
        edgelist_from_db <- get_edge_list_db(conMysql, pmids_in_db)
        
        # Update database with edgelist_not_from_db only if there is values in edgelist_not_from_db
        if(length(edgelist_not_from_db$target) != 0){
          insertEdgeList(conMysql,edgelist_not_from_db)
        }
        
        final_edge_list <- combineEdgeList(edgelist_not_from_db, edgelist_from_db)
        
      }else{
        # Got conMysql, lastUpdate = NOT NULL
        
        # get_pmids_date that are less than `lastUpdate`.
        # if `pmids_date` is NULL, then do not need to update. Then I can just pull all date from DB?
        pmids_date <- get_pmids_date(conMysql, lastUpdate)
        pmids_date <- pmids_date$target
        
        if(length(pmids_date) == 0){
          # This is for when get_pmids_date get nothing from DB.
          # This is the same as Got conMysql, lastUpdate = NULL
          
          # only keep the pmids that are not in the database already
          pmids_not_in_db <- check_pmids_in_db(conMysql, pmids)
          elink_not_in_db <- get_pmc_cited_in(pmids_not_in_db)
          edgelist_not_from_db <- generateEdgeList(elink_not_in_db)
          
          # find the matching pmids that are not in database
          match_pmids <- pmids %in% pmids_not_in_db
          # only keep the pmids that is in database
          pmids_in_db <- pmids[!match_pmids]
          edgelist_from_db <- get_edge_list_db(conMysql, pmids_in_db)
          
          # Update database with edgelist_not_from_db only if there is values in edgelist_not_from_db
          if(length(edgelist_not_from_db$target) != 0){
            insertEdgeList(conMysql,edgelist_not_from_db)
          }
          
          final_edge_list <- combineEdgeList(edgelist_not_from_db, edgelist_from_db)
        }else{
          # This is for when there is a result from get_pmids_date
          # The pmids from get_pmids_date have to be updated
          
          # only keep the pmids that are not in the database already
          pmids_not_in_db <- check_pmids_in_db(pmids)
          elink_not_in_db <- get_pmc_cited_in(pmids_not_in_db)
          edgelist_not_from_db <- generateEdgeList(elink_not_in_db)
          
          # get new edge list for pmids that needs to be update in database
          elink_pmids_date <- get_pmc_cited_in(pmids_date)
          edgelist_pmids_date <- generateEdgeList(elink_pmids_date)
          
          edgelist_not_from_db <- combineEdgeList(edgelist_not_from_db, edgelist_pmids_date)
          
          # find the matching pmids that are not in database
          match_pmids <- pmids %in% pmids_not_in_db
          # only keep the pmids that is in database
          pmids_in_db <- pmids[!match_pmids]
          edgelist_from_db <- get_edge_list_db(conMysql, pmids_in_db)
          
          insertEdgeList(conMysql, edgelist_not_from_db)
          
          final_edge_list <- combineEdgeList(edgelist_not_from_db, edgelist_from_db)
        }
        
      }
    
    }else{
      print("You 'con' argument is not a MariaDBConnection")
      # figure how to stop retrieveEdgeList() early
    }
    
    
  }else{
    # Only get edge list from NCBI because conMysql = NULL
    res <- get_pmc_cited_in(pmids)
    final_edge_list <- generateEdgeList(res)
  }
  final_edge_list
}