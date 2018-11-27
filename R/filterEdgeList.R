#' Edge list filtering.
#'
#' `filterEdgeList` filters an edge list by citation threshold

#' @details
#' This function returns an edge list having
#' target IDs with at least 'count' citations
#' 
#' @param e an edge list, as obtained from `generateEdgeList`
#' @parama count threshold value for citation counts
#' @return A list with the following objects:
#'         edgeList - the filtered edge list
#'         counts - a tibble with one column for target PMIDS and one column, 'n', containing the corresponding frequency of citations
#' @seealso \code{\link{generateEdgeList}} for generating an edge list, and 
#'          \code{\link{getCitationCounts}} for counting citation frequencies
#' 
#' @examples
#' 
#' # generates an edge list for multiple articles
#' res <- get_pmc_cited_in(c(21876761, 311,29463753))
#' e <- generateEdgeList(res)
#' e2 <- filterEdgeList(e, 5)

#' @export
filterEdgeList <- function(e, count) {
  counts <- getCitationCounts(e)
  target <- counts$Target[counts$n>=count] 
  e <- filter(e, Target %in% target)
  
  list(edgeList = e, counts = counts)
  
}
