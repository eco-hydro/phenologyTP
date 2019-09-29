#' @export
last_row <- function(x) {
    x[nrow(x), ]
}

#' which_max
#' 
#' @return c(value, pos)
#' 
#' @export
which_max <- function(x) {
    I <- which.max(abs(x))
    c(value = x[I], pos = I)
}

#' @export
yearlyDates <- function(year) {
    dates <- seq(ymd(year*1e4+0101), ymd(year*1e4+1231), "day")
}
