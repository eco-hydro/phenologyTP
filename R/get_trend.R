#' get_trend
#'
#' @param mat `[ngrid, ntime]`
#'
#' @export
get_trend <- function(mat, limits = NULL, prefix = "") {
    # min length is 6:
    len_valid <- rowSums2(!is.na(mat))
    I_valid <- which(len_valid >= 6)

    if (is.null(limits)) limits = length(I_valid)

    res <- foreach(x = t(mat[I_valid, ]), i = icount(limits), .combine = rbind) %do% {
        runningId(i, 1e5, N = length(I_valid), prefix = prefix)
        tryCatch(
            {
                mkTrend(x)[5:4] # adjusted slp and pvalue
            },
            error = function(e) {
                message(sprintf("[e] i = %5d, %s", i, e$message))
                c(NA_real_, NA_real_)
            }
        )
    }
    listk(I_valid, trend = res)
}
