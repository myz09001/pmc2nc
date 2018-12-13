#' Retrieve an edge list from the provided PMIDs
#'
#' `retrieveEdgeList` Retrieve an edge list from the provided PMIDs

#' @details

#' @param pmids a vector of PMIDs look-up.
#' @param batchSize the batch size to use.
#' @param conMysql connection to mysql as defined in `~/.my.cnf`.
#' @param lastUpdate string of a date used to find old PMIDs stored in database in YYYY-MM-DD format.
#' @return An edge list (data.frame) with one column for Target PMIDS and one column for source PMIDS.
#' 
#' 
#' @seealso \code{\link{get_pmc_cited_in}} for obtaining edge_list citation results
#' @seealso \code{\link{generateEdgeList}} for obtaining edge_list citation results
#'
#' @examples

#' @export

retrieveEdgeList <- function(pmids, batchSize = 200, conMysql = NULL, lastUpdate = NULL){
  if(!is.null(conMysql)){
    
    if(is(conMysql, "MariaDBConnection")){
      # This will check if the 2 tables exist and if not, will create them
      create_edge_list_table(conMysql)
      create_date_table(conMysql)
      
      if(is.null(lastUpdate)){
        # conMysql = TRUE, lastUpdate = NULL
        
        final_edge_list <- retrieveEdgeList1(pmids, batchSize, conMysql, lastUpdate)
        
        # This else if makes sure the lastUpdate is a valid date for MySQL
      }else if (!is.na(isDate(lastUpdate))){
        # conMysql = TRUE, lastUpdate = NOT NULL
        
        # get_pmids_date that are less than `lastUpdate`.
        # if `pmids_date` is NULL, then do not need to update. Then it is the same as conMysql = TRUE, lastUpdate = NULL
        pmids_date <- get_pmids_date(conMysql, lastUpdate)
        pmids_date <- pmids_date$Target
        
        if(length(pmids_date) == 0){
          # This is for when get_pmids_date get nothing from DB.
          # Therefor its the same as conMysql = TRUE, lastUpdate = NULL
          
          final_edge_list <- retrieveEdgeList1(pmids, batchSize, conMysql, lastUpdate)
          
        }else{
          # This is for when there is a result from get_pmids_date
          # The pmids from get_pmids_date have to be updated
          
          # only keep the pmids that are not in the database already
          pmids_not_in_db <- find_pmids_not_in_db(conMysql, pmids)
          
          if (length(pmids_not_in_db) != 0){
            elink_not_in_db <- get_pmc_cited_in(pmids_not_in_db, batchSize)
            edgelist_not_from_db <- generateEdgeList(elink_not_in_db)
          }else{
            # When everything is already in database so edgelist_not_from_db will be NULL
            edgelist_not_from_db <- NULL
          }
          
          # find the matching pmids that are not in database
          match_pmids <- pmids %in% pmids_not_in_db
          # only keep the pmids that is in database
          pmids_in_db <- pmids[!match_pmids]
          
          # need to filter pmids_date to only pmids I care about. Right now its getting ALL pmids that meet lastUpdate.
          match_pmids <- pmids_date %in% pmids_in_db
          pmids_date_keep <- pmids_date[match_pmids]
          

          # get new edge list for pmids that needs to be update in database
          elink_pmids_date <- get_pmc_cited_in(pmids_date_keep, batchSize)
          edgelist_pmids_date <- generateEdgeList(elink_pmids_date)
          
          # This will combine both edge list from NCBI and edge list from DB that needs to be updated
          edgelist_not_from_db <- combineEdgeList(edgelist_not_from_db, edgelist_pmids_date)
          
          
          ###### One last thing to change ###########
          # the order is messed up  with qry and insert. the insert needs to be done first so update pmid will clear the old data
          # pmids_in_db needs to remove the pmids from pmids_date_keep
          
          # qry DB for edge list
          edgelist_from_db <- get_edge_list_db(conMysql, pmids_in_db)
          
          # insert edge list not from DB
          insertEdgeList(conMysql, edgelist_not_from_db)
          
          final_edge_list <- combineEdgeList(edgelist_not_from_db, edgelist_from_db)
        }
        
      }else{
        print("lastUpdate input is not in YYYY-MM-DD format")
        final_edge_list <- NULL
      }
    
    }else{
      print("You 'conMysql' argument is not a MariaDBConnection")
      final_edge_list <- NULL
    }
    
  }else{
    # Only get edge list from NCBI because conMysql = NULL
    res <- get_pmc_cited_in(pmids, batchSize)
    final_edge_list <- generateEdgeList(res)
  }
}

# Because when get_pmids_date get nothing from DB will query the same way as conMysql = TRUE, lastUpdate = NULL,
# I made it into one function.
retrieveEdgeList1 <- function(pmids, batchSize, conMysql, lastUpdate){
  # only keep the pmids that are not in the database already
  pmids_not_in_db <- find_pmids_not_in_db(conMysql, pmids)
  
  if (length(pmids_not_in_db) != 0){
    elink_not_in_db <- get_pmc_cited_in(pmids_not_in_db, batchSize)
    edgelist_not_from_db <- generateEdgeList(elink_not_in_db)
  }else{
    # When everything is already in database so edgelist_not_from_db will be NULL
    edgelist_not_from_db <- NULL
  }
  
  # find the matching pmids that are not in database
  match_pmids <- pmids %in% pmids_not_in_db
  
  # only keep the pmids that is in database
  pmids_in_db <- pmids[!match_pmids]
  
  # qry DB for edge list
  edgelist_from_db <- get_edge_list_db(conMysql, pmids_in_db)
  
  # insert edge list not from DB
  insertEdgeList(conMysql, edgelist_not_from_db)

  final_edge_list <- combineEdgeList(edgelist_not_from_db, edgelist_from_db)
}



