coef_lm <- function(d) {
    varnames <- names(d)
    formula = as.formula(sprintf("%s ~ %s", varnames[1], paste(varnames[-1], collapse = " + ")))
    
    tryCatch({
        l <- lm(formula, d) %T>% tidy()
        r <- tidy(l)
        list(coef = r$estimate[-1], pvalue = r$p.value[-1])
    }, error = function(e) {
        message(sprintf('%s', e))
        NULL
    })
}

coef_pls <- function(d) {
    tryCatch({
        r <- plsreg1(d[, -1], d[, 1])
        list(coef = r$reg.coefs[-1], pvalue = r$VIP[2, ])
    }, error = function(e) {
        message(sprintf('%s', e))
        NULL
    })
}

sign_perc <- function(x, mask){
    sign <- sign(x)
    n <- length(x)
    pos <- sum(sign > 0 & mask)/n
    neg <- sum(sign < 0 & mask)/n
    listk(pos, neg)
}

get_regional_sign <- function(d, d_id, by, to_dt = TRUE) {
    if (!is.character(by)) by <- names(by)
    # nvar = length(by)
    data <- merge(d, d_id)
    d_val  <- data[is.finite(value), mean(value), by] %>% dcast2("variable", "V1")
    d_sign <- data[is.finite(value), sign_perc(value, mask), by]
    ncol <- ncol(d_sign)

    pos <- dcast2(d_sign[, .SD, .SDcols = setdiff(1:ncol, ncol)], "variable", "pos")
    neg <- dcast2(d_sign[, .SD, .SDcols = setdiff(1:ncol, ncol-1)], "variable", "neg")
    ans = listk(value = d_val, pos, neg)
    
    if (to_dt) ans <- do.call(cbind, ans)
    ans
}
