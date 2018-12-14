#' Retrieve an edge list from the provided PMIDs
#'
#' `retrieveEdgeList` Retrieve an edge list from the provided PMIDs

#' @details
#' Code overview.
#' 
#' 1. Check if there is a MariaDBConnection, if not, just get everything from NCBI
#' 
#' 2. If there is a MariaDBConnection, check for tables in DB and create them if they dont exist.
#' 
#' 3. Check if lastUpdate = NULL, if lastUpdate is NULL, run retrieveEdgeList1.
#' 
#' retrieveEdgeList1 - 1. query table edge list date for PMID in database 
#' 2. split the pmids list into pmids_in_db and pmids_not_in_db
#' 3. pmids_not_in_db search in NCBI and pmids_in_db search in DB
#' 4. insert edgelist of pmids_not_in_db into database
#'                           
#' 4. If lastUpdate != NULL, check which PMIDs are out of date. If no PMIDs are out of date, run retrieveEdgeList1.
#' 
#' 5. If there are out of date PMIDs, get list of pmids_in_db and seperate the list between pmids do and not need update
#'    Get a list of pmids that need to search NCBI if needed. Combine edge list from NCBI and pmids that need update. Then
#'    insert this conbinded edge list to database. Query database with the list of pmids that do not need update.

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
#' pmid <- 21876761
#' pmids1 <- c(21876761, 311, 29463753, 21876726)
#' 
#' # This will create tables and insert edge list from one pmid "21876761"
#' # This gets everything from NCBI if there is no database
#' res1 <- retrieveEdgeList(pmid, conMysql = con_mysql)
#' 
#' # This will not create new tables or insert edge list. This will just take everything from DB.
#' # res2 == res1
#' res2 <- retrieveEdgeList(pmid, conMysql = con_mysql)
#'
#' # This will not create new table. This will insert 29463753, 21876726
#' # This will qry db for 21876761
#' # result will be like res1 with the combine 29463753, 21876726 edge list
#' res3 <- retrieveEdgeList(pmids1, conMysql = con_mysql)
#'
#' # This will insert Source with PMID 1000001 This will use to test lastUpdate
#' # insertEdgeList will delete everything with "Target" = 21876761 and insert e1
#' e1 <- data.frame("Source" = 1000001, "Target" = 21876761 )
#' insertEdgeList(con_mysql, e1)
#'
#' # This should only return one edge list (1000001, 21876761)
#' res4 <- retrieveEdgeList(pmid, conMysql = con_mysql)
#'
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
          
          # need to filter pmids_date that users are looking for. Right now its getting ALL pmids that meet lastUpdate.
          match_pmids <- pmids_date %in% pmids_in_db
          pmids_date_update <- pmids_date[match_pmids]
          
          # now removing the PMIDs in pmids_in_db that are in pmids_date_update because pmids_in_db do not need to be updated.
          match_pmids <- pmids_in_db %in% pmids_date_update
          pmids_in_db <- pmids_in_db[!match_pmids]
          

          # get new edge list for pmids that needs to be update in database
          elink_pmids_date <- get_pmc_cited_in(pmids_date_update, batchSize)
          edgelist_pmids_date <- generateEdgeList(elink_pmids_date)
          
          
          # This will combine both edge list from NCBI and edge list from DB that needs to be updated
          edgelist_not_from_db <- combineEdgeList(edgelist_not_from_db, edgelist_pmids_date)
          
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



