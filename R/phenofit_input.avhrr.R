
#' predict_date
#' @export
#' 
#' @examples
#' date_AVHRR <- get_date_AVHRR()
#' predict_date(date_AVHRR, date_AVHRR$I)
predict_date <- function(d_date, xout){
    # only integer obj returned
    # origin(0): '1970-01-01'
    approx(d_date$I, d_date$date, xout)$y %>% as.Date(origin = "1969-12-31")
}

#' phenofit_input.avhrr
#' 
#' prepare input for phenofit obj
#' @export
phenofit_input.avhrr <- function(mat_y, mat_qc, d_date, wmax = 0.8, 
    I_st, st, outfile)
{
    # if (missing(I_st)) {
        # I_st <- 1:nrow(mat_y)
        sites <- seq_along(I_st) %>% as.character()
    # } else {
    #     # caution: global variable st
    #     sites <- as.character(st$site)
    # }

    lst <- list(y = mat_y[I_st, ], 
          qc = mat_QC[I_st, ])

    df <- foreach(mat = lst, name = names(lst)) %do% {
        res <- t(mat) %>% data.table() %>% 
            set_colnames(sites) %>% 
            cbind(t = d_date$date, .) %>% 
            melt("t", variable.name = "site", value.name = name)
    } %>% {merge(.[[1]], .[[2]])}
    df[, c("QC_flag", "w") := qc_NDVI3g(qc, wmax = 0.8)]

    if (!missing(outfile)) {
        fwrite(df, outfile)
    }
    return(df)
}


check_snow <- function(y, t){
    # 11 - 3月
}
