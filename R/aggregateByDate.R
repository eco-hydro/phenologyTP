#' aggregateByDate
#' 
#' @param format '\%Y-\%m-\%d': year, month, day
#' 
#' @export
aggregateByDate <- function(mat, dates, format){
    if (missing(format)) { format <- "%m" } 
    dates_new <- format(dates, format)

    res <- llply(unique(dates_new) %>% set_names(., .), function(i){
        I <- which(dates_new == i)
        rowMeans2(x, cols = I)
    }) %>% do.call(cbind, .)    
    res
}


#' @importFrom matrixStats rowMeans2
#' @export
multiYear_mean <- function(mat, nptperyear = 24){
    dims  <- dim(mat)
    ndim  <- length(dims)
    ntime <- dims[ndim] # time in last column
    
    array(mat, c(dims[-ndim], nptperyear, ntime/nptperyear)) %>% array_mean(ndim + 1)
}

#' fill_grid
#' 
#' `row` should be in the first column
#' 
#' @export
fill_grid <- function(grid, d){
    ngrid <- nrow(grid)
    I <- match(seq_len(ngrid), d$row)

    grid@data <- d[I, -1] %>% data.frame()
    grid
}
