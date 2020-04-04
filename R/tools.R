set_year_names <- . %>% set_names(., str_extract(basename(.), "\\d{4}"))

#' @import sp
as_SpatialPixelsDataFrame <- function(x) {
    as(x, "SpatialPixelsDataFrame")
}

as_SpatialGridDataFrame <- function(x) {
    as(x, "SpatialGridDataFrame")
}

# load draw.colorkey
#' @importFrom Ipaper draw.colorkey flipud
suppressWarnings({
    environment(draw.colorkey) <- environment(lattice::xyplot)
    assignInNamespace("draw.colorkey", draw.colorkey, ns = "lattice")
})

# load_all("E:/Research/cmip5/Ipaper")
add_column_id <- function(d) {
    if (!("I" %in% colnames(d))) {
        d %>% cbind(I = 1:nrow(.), .)
    } else {
        d
    }
}

transpose <- purrr::transpose

#' tidy_PML
#' 
#' @examples
#' lst_dynamic <- llply(files_dynamic, readGDAL, band = 1:4)
tidy_PML <- function(lst, grid) {
    lst %>% map(~.x@data %>% set_names(bandNames[1:4])) %>%
        resample_lst(., grid) %>%
        transpose() %>% map(~do.call(cbind, .))
}




yearly_date <- function(year, dn = 4) {
    doy <- seq(1, leap_year(year) + 365, dn)
    as.Date(sprintf("%d-%03d", year, doy), "%Y-%j")
}

seq_date_dn <- function(start, end, dn = 4) {
    start = ymd(start)
    end   = ymd(end)

    year_start = year(start)
    year_end   = year(end)
    ans <- lapply(year_start:year_end, yearly_date, dn) %>% do.call(c, .)
    ans %>% .[. >= start & . <= end]
}

