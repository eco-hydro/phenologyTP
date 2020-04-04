get_anorm <- function(lst) {
    first = lst[[1]]@data
    res = foreach(l = lst[-1], i = icount()) %do% {
        d = data.table(l@data - first) %>% cbind(I = 1:nrow(.), .)
    } 
    melt_list(res, "year")
}

get_anorm_lc <- function(lst) {
    first = lst[[1]]
    res = foreach(l = lst[-1], i = icount()) %do% {
        d = data.table(l - first) %>% cbind(I = 1:nrow(.), .)
    } 
    melt_list(res, "year")
}
