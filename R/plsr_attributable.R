#' This function only suits for EOS model, Tmin, Tmax, Prec, Srad and SOS as INPUT
#' 
#' @param d Pre-season data.frame returned by [get_preseason()].
#' 
#' @export
plsr_attributable <- function(d, include.fitted = TRUE) {
    I      <- 1:nrow(d)
    I_nona <- is.na(d) %>% matrixStats::rowSums2(na.rm=TRUE) %>% {which(. == 0)}
    d <- d[I_nona, ] %>% as.matrix()
    d <- d %>% as.matrix()

    X <- d[, 1:5]             # METE + SOS
    Y <- d[, 6, drop=FALSE]   # EOS 

    # 1. include SOS
    m <- plsreg1_adj(X, Y, comps = 2, autoVars = FALSE, include.fitted = include.fitted)$init %>% 
        plsr_fix_ypred(I, I_nona)
    
    # 2. drop SOS
    m_nonSOS <- plsreg1_adj(X[, -5], Y, comps = 2, autoVars = FALSE, include.fitted = include.fitted)$init %>% 
        plsr_fix_ypred(I, I_nona)

    list(SOS = m, `Non-SOS` = m_nonSOS)
}
