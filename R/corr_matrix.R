#' corr_matrix
#' 
#' Correlation of each pixel.
#' 
#' @param X `[time, grid]`, the dimension of X and Y should be same.
#' @param Y `[time, grid]`
#' 
#' @import foreach iterators
#' @importFrom data.table data.table
#' 
#' @export 
corr_matrix <- function(X, Y){
    d_corr <- foreach(x = X, y = Y, i = icount(),
        .combine = "rbind") %do% {
        tryCatch({
            xx <- pracma::detrend(x)
            yy <- pracma::detrend(y)

            I <- which(!(is.na(xx) | is.na(yy)))
            n <- length(I)
            r <- cor.test(xx, yy, use = "pairwise.complete.obs")
            c(R = r$estimate[[1]], pvalue = r$p.value, n = n)
        }, error = function(e){
            message(sprintf("[e] %d: %s", i, e$message))
            c(R = NA_real_, pvalue = NA_real_, n = NA_real_)
        })
    } %>% data.table()
    return(d_corr)
}
