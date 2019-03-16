#' @export
mask_outlier <- function(x) {
    sd <- sd(x, na.rm = TRUE)
    mean <- mean(x, na.rm = TRUE)

    I_bad <- which(abs(x - mean) >= 3*sd)
    x[I_bad] <- NA
    x
}
