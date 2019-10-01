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

#' @export
mask_outlier <- function(x) {
    sd <- sd(x, na.rm = TRUE)
    mean <- mean(x, na.rm = TRUE)

    I_bad <- which(abs(x - mean) >= 3*sd)
    x[I_bad] <- NA
    x
}

