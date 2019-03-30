label_fun <- function(x, include.sd=FALSE){
    mu <- median(x, na.rm = T)
    sd <- sd(x, na.rm = T)

    r <- data.frame(y = mu)
    r$label <- ifelse(include.sd, sprintf("%.1f±%.1f", mu, sd), sprintf("%.1f", mu)) 
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
