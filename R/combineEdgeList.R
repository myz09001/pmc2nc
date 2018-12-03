#' Combine 2 edge lists together
#'
#' `edgelist_1` and `edgelist_2` must come from the result of `generateEdgeList` or `get_edge_list_db`

#' @details
#' `edgelist_1` and `edgelist_2` must come from the result of `generateEdgeList` or `get_edge_list_db`
#'
#' @param edgelist_1 edge_list results, as obtained from `generateEdgeList` (see details)
#' @param edgelist_2 edge_list results, as obtained from `generateEdgeList` (see details)
#' @return combineEdgeList() will return one dataframe after joining two edge list vertically.

#' @examples
#'
#' # place holder



#' @export
#' 
combineEdgeList <- function(edgelist_1, edgelist_2){
  res <- rbind(edgelist_1, edgelist_2)
}