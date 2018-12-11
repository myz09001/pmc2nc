#' Collect additional citations from the supplied edge list.
#'
#' `edge_list_expansion` insert the edge list from the results of `generateEdgeList`

#' @details
#' `edge_list` must come from the result of generateEdgeList
#'
#' @param edge_list edge_list results, as obtained from `generateEdgeList` (see details)
#' @return An edge list (data.frame) with one column for target PMIDS and one column for source PMIDS.
#' @seealso \code{\link{get_pmc_cited_in}} for obtaining elink citation results
#' @seealso \code{\link{generateEdgeList}} for obtaining elink citation results
#'
#' @examples
#'
#' # Collect addition citations from the supplied 'e2'
#' res1 <- get_pmc_cited_in(21876761)
#' e2 <- generateEdgeList(res1)
#' edge_list_expansion(e2)


#' @export



edge_list_expansion <- function(edge_list){
  # find the pmids that exist in both source and target
  match_pmids <- edge_list$Source %in% edge_list$Target

  # keep the pmids that is unique to edget_list$source because edge_list$target has been searched already
  pmids <- edge_list$Source[!match_pmids]

  # keep only the unique pmids
  pmids <- unique(pmids)

  res <- get_pmc_cited_in(pmids)
  
  res <- generateEdgeList(res)
}
