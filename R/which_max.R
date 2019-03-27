#' which_max
#' 
#' @return c(value, pos)
#' 
#' @export
which_max <- function(x) {
    I <- which.max(abs(x))
    c(value = x[I], pos = I)
}
