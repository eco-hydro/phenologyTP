#' @importFrom matrixStats rowMeans2 rowMins rowMaxs
#'  
#' @export
apply_3d <- function(array, dim, FUN = rowMeans2, ...) {
    dims <- dim(array)
    ndim <- length(dims) # dimensions
    
    I_dims     <- setdiff(1:ndim, dim) # dimensions order
    dims_head  <- dims[I_dims]         # header dimensions 
    if (dim != length(dims)){
        array %<>% aperm(c(I_dims, dim)) 
    } 
    ans <- array(array, dim = c(prod(dims_head), dims[dim])) %>% 
        FUN(..., na.rm = TRUE) %>% 
        array(dim = dims_head)
    ans
}


#' @importFrom matrixStats rowMeans2
#' @export
multiYear_mean <- function(mat, nptperyear = 24){
    dims  <- dim(mat)
    ndim  <- length(dims)
    ntime <- dims[ndim] # time in last column
    
    array(mat, c(dims[-ndim], nptperyear, ntime/nptperyear)) %>% array_mean(ndim + 1)
}
