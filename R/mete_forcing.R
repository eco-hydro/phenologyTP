#' @export
read_forcing <- function(files, I_grid){
    range <- c(73, 105, 25, 40)

    years <- basename(files) %>% str_extract("(?<=_)\\d{4}") %>% as.numeric()
    
    varname <- basename(files[1]) %>% str_extract(".{4}(?=_)") 
    offset = 0; scale = 1
    
    read_file <- function(file, I_grid){
        if (varname == "temp") {
            offset = -273.15
        } else if (varname == "prec") {
            scale = 24
        } 
        
        r <- ncread_cmip5(file, range = range, ntime = -1, 
                          convertTo2d = FALSE, 
                          adjust_lon = FALSE, delta = 0, 
                          offset = offset, scale = scale)
        mat <- flipud(r$data) # x[, ncol(x):1], in Rcmip5
        if (!missing(I_grid)) {
            mat <- array_3dTo2d(mat, I_grid)
        }
        mat
    }
    llply(files, read_file, I_grid = I_grid, .progress = "text") %>% set_names(years)
}


check_data <- function(mat){
    x <- mat[,1]
    gridclip2_10@data <- data.frame(x)
    p <- spplot(gridclip2_10)
    p
}
