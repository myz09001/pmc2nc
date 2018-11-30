#' Counts the number of citations for each target ID.
#'
#' `getCitationCounts` counts the number of citations for each target ID in an edge list

#' @details
#' `elink` must be one of the following: a list containing one elink object, 
#' with list name corresponding to target PMID; a list of elink objects,
#' with names corresponding to target PMIDs, or a list of any combination
#' of the above. These corresond to a single PMID, a single batch,
#' and multiple batches, respectively.
#' 
#' @param e an edge list, as obtained from `generateEdgeList` 
#' @return A tibble with one column for target PMIDS and one column, 'n', containing the corresponding frequency of citations
#' @seealso \code{\link{generateEdgeList}} for generating an edge list
#' 
#' @examples
#' 
#' # generates an edge list for multiple articles
#' res <- get_pmc_cited_in(c(21876761, 311,29463753))
#' e <- generateEdgeList(res)
#' counts <- getCitationCounts(e)

#' @export
getCitationCounts <- function(e) {
  require(dplyr)
  group_by(e, Target) %>% count()
}
