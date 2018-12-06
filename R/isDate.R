#' isDate will check if date input work with MySQL
#'
#' `isDate` will try to format the input into a format recognizable to MySQL. If the formating fails, it will return NA

#' @details
#' `edge_list` must come from the result of generateEdgeList
#'
#' @param mydate string to be tested
#' @param date.format the date format
#' @return isDate() returns true or false if the value is a date
#'
#' @examples
#'
#' # place holder
#' 
#' 
isDate <- function(mydate) {
  # This is the date format that matches MySQL. Y = year with 4 digits, m = month with 2 digits, d = day with 2 digits
  date.format = "%Y-%m-%d"
  # This will try to convert mydate into the corresponding date format and will return NA if it fails.
  res <- as.Date(mydate, date.format)
}