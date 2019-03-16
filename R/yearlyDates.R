#' @export
yearlyDates <- function(year) {
    dates <- seq(ymd(year*1e4+0101), ymd(year*1e4+1231), "day")
}
