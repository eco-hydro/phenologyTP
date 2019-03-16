#' @name critical
#' @title critical
#' 
#' @param n the number of samples
#' @param gn the number of given variables
#' 
#' @references
#' [1] Chen Yanguang (2012), Geographical Data analysis with MATLAB. \cr
#' [2] Kim, S. (2015) ppcor: An R Package for a Fast Calculation to Semi-partial 
#' Correlation Coefficients. Communications for Statistical Applications and Methods, 
#' 22(6), 665-674. \cr
#' [3] pcor p.value calculation in ppcor. \cr
#' 
#' @examples
#' critical_pcor(34, 2)
#' critical_pcor2(34, 2)
NULL

#' @rdname critical
#' @export
critical_pcor <- function(n, gn, alpha = c(0.1, 0.05, 0.01)){
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

# autocorrelation critical values
#' @rdname critical
#' @export
critical_autor <- function(n, alpha = c(0.1, 0.05, 0.01)) {
    Rc_acf <- qnorm(1 - alpha/2)/sqrt(n)
}