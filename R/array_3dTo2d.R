#' @export
array_3dTo2d <- function(array, I_grid){
    array <- fliplr.3d(array)
    dim <- dim(array)
    dim(array) <- c(prod(dim[1:2]), dim[3])
    array <-  array[I_grid, ]
    return(array)
}
