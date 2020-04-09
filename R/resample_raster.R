#' @importFrom raster raster resample
#' @export
resample2_10deg <- function(grid_cliped, df, range){
    cellsize_10 <- 1/10
    # lat_10 <- seq(range[1]+cellsize_10/2, range[2], cellsize_10)
    # lon_10 <- seq(range[3]+cellsize_10/2, range[4], cellsize_10)
    grid_10 <- get_grid(range, cellsize_10)
    
    data <- fill_grid(grid_cliped, df)@data %>% data.frame()
    # spplot(gridclip, 1:4)
    years <- 1982:2015
    foreach(year = years, i = icount()) %do% {
        grid_cliped@data <- data[, i, drop = FALSE] # %>% data.frame()
        r <- resample(raster(grid_cliped), raster(grid_10))
        
        vals <- r@data@values[I_grid_10]
        vals
    } %>% set_names(years) %>% as.data.frame()
}

#' @seealso [raster::resample()]
#' @export 
resample_raster <- function(brick, grid, method="bilinear", ...) {
    resample(brick(grid), raster(grid), method, ...)
}

resample_grid <- function(grid, data = NULL, fact=5, fun=mean) {
    if (!is.null(data)) grid@data <- data

    r <- brick(grid)
    if (fact > 1) {
        r <- aggregate(r, fact, fun) # 0.5 deg
    }
    as_SpatialGridDataFrame(r)
}

# lc_trends
# - `I_valid`
# - `trend`
resample_lst <- function(lst, grid, scale = 1) {
    ans <- foreach(l = lst, i = icount()) %do% {
        runningId(i)

        if (is.data.frame(l)) {
            d = resample_grid(grid, l)@data
        } else {
            grid2 <- grid[l$I_valid, ]
            grid2@data <- as.data.frame(l$trend*scale)
            d = resample_grid(grid2)@data
        }
    }
    ans
}


#' fill_grid
#'
#' `row` should be in the first column
#'
#' @export
fill_grid <- function(grid, d) {
    ngrid <- nrow(grid)
    I <- match(seq_len(ngrid), d$row)

    grid@data <- d[I, -1] %>% data.frame()
    grid
}

fill_grid2 <- function(ngrid, d) {
    # ngrid <- nrow(grid)
    ind <- seq_len(ngrid)
    I <- match(seq_len(ngrid), d[[1]])
    # browser()
    ans <- d[I, ]
    ans[[1]] <- ind
    ans
    # grid
}
