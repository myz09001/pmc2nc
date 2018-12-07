#' isDate will format date input to work with MySQL
#'
#' `isDate` will try to format the input into a format recognizable to MySQL. If the formating fails, it will return NA

#' @details
#' `isDate` must come from the result of generateEdgeList
#'
#' @param mydate string to be tested
#' @return isDate() returns true or false if the value is a date
#'
#' @examples
#' 
#' # This will print 2001-01-09"
#' date<- "2001-1-9"
#' isDate(date)
#'
#' #This will return NA
#' date <- "209-01-9999"
#' isDate(date)
#' 

#' @export
isDate <- function(mydate) {
  # This is the date format that matches MySQL. Y = year with 4 digits, m = month with 2 digits, d = day with 2 digits
  date.format = "%Y-%m-%d"
  
  # This will try to convert mydate into the corresponding date format and will return NA if it fails.
  res <- as.Date(mydate, date.format)
}