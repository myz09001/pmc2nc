#' Combine 2 edge lists together
#'
#' `edgelist_1` and `edgelist_2` must come from the result of `generateEdgeList` or `get_edge_list_db`

#' @details
#' `edgelist_1` and `edgelist_2` must come from the result of `generateEdgeList` or `get_edge_list_db`.
#' If either edgelist_1 or edgelist_2 are NULL, only pass the edge list with values in it.
#'
#' @param edgelist_1 edge_list results, as obtained from `generateEdgeList` (see details)
#' @param edgelist_2 edge_list results, as obtained from `generateEdgeList` (see details)
#' @return combineEdgeList() will return one dataframe after joining two edge list vertically.

#' @export
#' 
combineEdgeList <- function(edgelist_1, edgelist_2){
  if(length(edgelist_1) != 0 && length(edgelist_2) == 0){
    print("One edge list does not have any values, returning the one with values")
    res <- edgelist_1
  }else if (length(edgelist_1) == 0 && length(edgelist_2) != 0){
    print("One edge list does not have any values, returning the one with values")
    res <- edgelist_2
  }else if(length(edgelist_1) == 0 && length(edgelist_2) == 0){
    print("Both edge list is empty. Returning NULL")
    res <- NULL
  }else{
    print("Combining two edge list now.")
    res <- rbind(edgelist_1, edgelist_2)
  }
}