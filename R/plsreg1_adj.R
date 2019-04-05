#' plsreg1_adj
#' 
#' @examples
#' \dontrun{
#' plsreg1_adj(X, Y, comps = 2, autoVars = TRUE, nminVar = 2, minVIP = 0.8)
#' }
#' @export
plsreg1_adj <- function(X, Y, comps = 2, 
    autoVars = TRUE, nminVar = 2, minVIP = 0.8, 
    include.fitted = FALSE, ...)
{
    varnames <- varnames0 <- colnames(X)
    # select_model
    m_pls <- plsdepot::plsreg1(X[, varnames, drop = FALSE], Y, comps = comps, crosval = TRUE)
    r_init <- PLS_performIndex(m_pls, varnames0, include.fitted, I_nona)
    
    if (autoVars) {
        repeat({
            VIP <- last_row(m_pls$VIP)
            # Q2  <- last_row(m_pls$Q2)                
            if (length(VIP) >= (nminVar + 1) && any(VIP <= minVIP)) {
                I_rm <- which.min(VIP)    
                varnames <- varnames[-I_rm]
                m_pls <- plsdepot::plsreg1(X[, varnames, drop = FALSE], Y, 
                    comps = comps, crosval = TRUE)
            } else {
                break
            }
        })
        # only init and the last kept
        r_last <- PLS_performIndex(m_pls, varnames0, include.fitted, I_nona)
        r <- list(init = r_init, last = r_last) # r is improved        
        purrr::transpose(r) %>% map(~do.call(rbind, .) %>% 
            data.frame() %>% set_rownames(NULL) %>% 
            cbind(type = c("init", "last"), .))
    } else {
        list(init = r_init) # r is improved        
    }
}

fill_missingVar <- function(x, varnames0){
    n <- length(varnames0)
    ans <- rep(NA_real_, n) %>% set_names(varnames0)
    I <- match(names(x), varnames0)
    ans[I] <- x
    ans
}

#' @export
PLS_performIndex <- function(obj, varnames0, include.fitted = FALSE, ...){
    Xx <- obj$INPUT$Xx
    Yy <- obj$INPUT$Yy
    mu.x <- attributes(Xx)$"scaled:center" #%>% as.matrix(ncol = 1)
    sd.x <- attributes(Xx)$"scaled:scale"  #%>% as.matrix(ncol = 1)
    mu.y <- attributes(Yy)$"scaled:center"
    sd.y <- attributes(Yy)$"scaled:scale"

    reg.coefs <- obj$reg.coefs[-1] # rm intercept
    std.coefs <- obj$std.coefs

    n <- nrow(Xx)
    ypred2 <- rep(NA_real_, n)
    ypred  <- obj$y.pred
    ypred2[as.numeric(names(ypred))] <- ypred

    # slope need * nyear = delta  (linear regression used here)
    # Note that: Xx and Yy has been scaled.
    attribute_change <- c(
        slope(Yy)*sd.y, # EOS 
        fill_missingVar(slope(Xx)*sd.x*reg.coefs, varnames0)) 

    ans <- list(
        reg.coefs = reg.coefs,
        std.coefs = std.coefs,
        VIP = last_row(obj$VIP), 
        attribute_change = attribute_change, 
        Q2  = last_row(obj$Q2))

    if (include.fitted) {
        ans$ypred <- ypred
    }
    ans[1:3] %<>% map(fill_missingVar, varnames0)
    ans
}

#' @export
plsr_fix_ypred <- function(obj, I, I_nona){
    ypred <- obj$ypred
    ans <- I*NA_real_
    ans[I_nona] <- ypred
    
    obj$ypred <- ans
    obj
}
