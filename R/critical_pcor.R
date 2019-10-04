#' @name critical
#' @title Critical value of Partial Correlation
#' 
#' @description
#' - `critical_autor`: critical value of autocorrelation
#' - `critical_pcor` : critical value of partial correlation
#' 
#' @param n the number of samples
#' @param gn the number of given variables. When `gn = 0`, the returned values are
#' critical values of pearson correlation.
#' @param alpha numeric vector, significant level
#' 
#' @references
#' 1. Chen Yanguang (2012), Geographical Data analysis with MATLAB. \cr
#' 2. Kim, S. (2015) ppcor: An R Package for a Fast Calculation to Semi-partial 
#' Correlation Coefficients. Communications for Statistical Applications and Methods, 
#' 22(6), 665-674. \cr
#' 3. pcor p.value calculation in ppcor. \cr
#' 
#' @examples
#' critical_pcor(34, 2)
#' critical_pcor2(34, 2)
NULL

#' @rdname critical
#' @export
critical_pcor <- function(n, gn = 0, alpha = c(0.1, 0.05, 0.01)){
    freedom <- n - 2 - gn # the number of given variables is 3
    Rc_cor <- sqrt(qf(1 - alpha, 1, freedom)/(qf(1 - alpha, 1, freedom) + freedom))
    Rc_cor
}

#' @rdname critical
#' @export
critical_pcor2 <- function(n, gn, alpha = c(0.1, 0.05, 0.01)){
    freedom <- n - 2 - gn # the number of given variables is 3
    # Rc = sqrt(qf(1 - alpha, 1, freedom)/(qf(1 - alpha, 1, freedom) + freedom))  # 0.11215
    # [1] 0.3232835 0.3808629 0.4869316

    ## based on Kim, S. (2015) ppcor: An R Package for a Fast Calculation to Semi-partial Correlation Coefficients.
    #       Communications for Statistical Applications and Methods, 22(6), 665-674.
    Rc = (-qt(alpha/2, freedom)/sqrt(freedom)) %>% {sin(atan(.))} #%>% print
    return(Rc)
}

#' @rdname critical
#' @export
critical_autor <- function(n, alpha = c(0.1, 0.05, 0.01)) {
    Rc_acf <- qnorm(1 - alpha/2)/sqrt(n)
}

#' @export
brks_pcor <- function(n = 34){
    brks <- critical_pcor(n, 3) %>% c(1) %>% c(-rev(.), 0, .) %>% round(3)
    brks
}


#' @export
get_R2_brks <- function(n = 34) {
    critical_pcor(n, 0) %>% c(., 1) %>% 
        c(-1, 0, .) %>% 
        round(3)
        # c(-rev(.), 0, .) %>% 
}
