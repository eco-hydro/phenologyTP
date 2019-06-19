#' @export
stat_sd <- function(x){
    x <- x[!is.na(x)]
    y <- mean(x)
    sd <- sd(x)
    list(y = y, ymin = y-sd, ymax = y+sd, sd = sd)
}

#' @export
stat_5p <- function(x) {
    tryCatch({
        boxplot.stats(x)$stats    
    }, error = function(){
        rep(NA_real_, 5)
    }) %>% 
        set_names(c("ymin", "lower", "middle", "upper", "ymax")) %>%
        as.list()
}
