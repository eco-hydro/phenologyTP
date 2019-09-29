label_fun <- function(x, include.sd=FALSE){
    md <- median(x, na.rm = T)
    sd <- sd(x, na.rm = T)

    r <- data.frame(y = md, sd = sd)
    r$label <- ifelse(include.sd, sprintf("%.1f±%.1f", md, sd), sprintf("%.1f", md)) 
    r
}

#' @export
label_sd <- function(x){
    label_fun(x, include.sd = TRUE)
}

#' @export
label_median <- function(x){
    label_fun(x, include.sd = FALSE)
}

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
