#' @export
last_row <- function(x) {
    x[nrow(x), ]
}

#' which_max
#' 
#' @return c(value, pos)
#' 
#' @export
which_max <- function(x) {
    I <- which.max(abs(x))
    c(value = x[I], pos = I)
}

#' @export
yearlyDates <- function(year) {
    dates <- seq(ymd(year*1e4+0101), ymd(year*1e4+1231), "day")
}

#' @export
mask_outlier <- function(x) {
    sd <- sd(x, na.rm = TRUE)
    mean <- mean(x, na.rm = TRUE)

    I_bad <- which(abs(x - mean) >= 3*sd)
    x[I_bad] <- NA
    x
}

path_mnt2win <- function(path) {
    path <- normalizePath(path)
    if (substr(path, 1, 5) == "/mnt/") {
        path = substr(path, 6, nchar(path)) %>% str_replace("/", ":/")    
    }
    path
}

#' If windows style, then convert to WSL style
#' 
#' @export
check_file <- function(path){
    if (.Platform$OS.type == "unix") {
        pos <- str_locate(path, ":")[1, 1]
        if (!is.na(pos)) {
            pan  <- substr(path, 1, 1)
            path <- paste0("/mnt/", tolower(pan), substr(path, 3, nchar(path)) )
        }
    }
    path
}

# load draw.colorkey
suppressWarnings({
    environment(draw.colorkey) <- environment(lattice::xyplot)
    assignInNamespace("draw.colorkey", draw.colorkey, ns="lattice")  
})