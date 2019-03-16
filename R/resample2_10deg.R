#' @importFrom raster raster resample
#' @export
resample2_10deg <- function(gridclip, df){
    cellsize_10 <- 1/10
    lat_10 <- seq(range[1]+cellsize_10/2, range[2], cellsize_10)
    lon_10 <- seq(range[3]+cellsize_10/2, range[4], cellsize_10)
    grid_10 <- get_grid(range, cellsize_10)
    
    data  <- fill_grid(gridclip, df)@data %>% data.frame()
    # spplot(gridclip, 1:4)
    years <- 1982:2015
    foreach(year = years, i = icount()) %do% {
        gridclip@data <- data[, i, drop = FALSE] # %>% data.frame()
        r <- resample(raster(gridclip), raster(grid_10))
        
        vals <- r@data@values[I_grid_10]
        vals
    } %>% set_names(years) %>% as.data.frame()
}