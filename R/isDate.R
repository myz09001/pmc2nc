#' isDate will format date input to work with MySQL
#'
#' `isDate` will try to format the input into a format recognizable to MySQL. If the formating fails, it will return NA

#' @details
#' `isDate` will try to format the input in YYYY-MM-DD and it will return NA if it cannot format the input.
#'
#' @param mydate string to be formated.
#' @return isDate() returns date in the format of YYYY-MM-DD. Returns NA if it fails to format.
#'
#' @examples
#' 
#' # This will print 2001-01-09"
#' date <- "2001-1-9"
#' <- isDate(date)
#'
#' # This will return NA
#' date <- "209-01-9999"
#' res <- isDate(date)
#' 

#' @export
isDate <- function(mydate) {
  if(!is.null(mydate)){
    # This is the date format that matches MySQL. Y = year with 4 digits, m = month with 2 digits, d = day with 2 digits
    date.format = "%Y-%m-%d"
    
    # This will try to convert mydate into the corresponding date format and will return NA if it fails.
    res <- as.Date(mydate, date.format)
  }else{
    print("isDate: Input is NULL")
    res <- NA
  }
}