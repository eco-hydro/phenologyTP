#' @export
fill_grid <- function(grid, d){
    ngrid <- nrow(grid)
    I <- match(seq_len(ngrid), d$row)
    
    grid@data <- d[I, -1] %>% data.frame()
    grid
}
