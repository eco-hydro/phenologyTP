
#' aggregateByDate
#' @param format "%Y-%m-%d": year, month, daty
#' 
#' @export
aggregateByDate <- function(mat, dates, format = "%m"){
    dates_new <- format(dates, format)

    res <- llply(unique(dates_new) %>% set_names(., .), function(i){
        I <- which(dates_new == i)
        rowMeans2(x, cols = I)
    }) %>% do.call(cbind, .)    
    res
}
