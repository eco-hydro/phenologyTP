## 对该程序进行改进，优化挑选变量的方法
#  Writed By Dongdong Kong, 2017-01-07
#  ------------------------------------
#  origin limq2 = 0.0975, 2017-01-15
#  
#' plsreg1
#' 
#' @param crosval Logical indicating whether 10 fold cross-validation should be 
#' performed (TRUE by default). No cross-validation is done, if there is 
#' missing data or if there are less than 10 observations.
plsreg1 <- function (predictors, response, comps = 2, crosval = TRUE, limq2 = 0.05) 
{
    X = as.matrix(predictors)
    n = nrow(X)
    p = ncol(X)
    if (p < 2) stop("predictors must contain more than one column")
    if (is.null(colnames(X))) colnames(X) = paste0("X", 1:p)
    if (is.null(rownames(X))) rownames(X) = 1:n
    Y = as.matrix(response)
    if (ncol(Y) != 1) stop("\nresponse must be a single variable")
    if (any(is.na(response))) stop("\nresponse must not contain missing values")
    if (nrow(X) != nrow(Y)) stop("\npredictors and response have different number of rows")
    if (is.null(colnames(Y))) colnames(Y) = "Y"
    if (is.null(rownames(Y))) rownames(Y) = 1:n
    na.miss <- ifelse(any(is.na(X)), TRUE, FALSE)

    if (!is.null(comps)) {
        nc = comps
        #nc >= 1, modified 2017-01-15
        if (mode(nc) != "numeric" || length(nc) != 1 || nc <= 1 || (nc%%1) != 0 || nc > min(n, p)) nc = min(n, p)
        if (nc == n) nc = n - 1
    } else {
        if (na.miss) {
            crosval = FALSE
            nc = 2
        } else {
            crosval <- ifelse(n>=10, TRUE, FALSE)
            nc = min(n, p)
        }
    }
    if (!is.logical(crosval)) crosval = FALSE
    Xx = scale(X)
    Yy = scale(Y)
    ## 进行计算时已经进行了标准化处理
    X.old = Xx
    Y.old = Yy
    Hot  <- Th <- Uh <- matrix(NA, n, nc)
    Ph   <- Wh <- matrix(NA, p, nc)
    ch   <- rep(NA, nc)
    hlim <- rep(NA, nc)
    if (crosval) {
        RSS = c(n - 1, rep(NA, nc))
        PRESS = rep(NA, nc)
        Q2 = rep(NA, nc)
        sets_size = c(rep(n%/%10, 9), n - 9 * (n%/%10))
        #Q2, PRESS et.al, changes' reason
        obs = 1:n
        # obs = sample(1:n, size = n)#2017-01-15
        segments = vector("list", length = 10)
        ini = cumsum(sets_size) - sets_size + 1
        fin = cumsum(sets_size)
        for (k in 1:10) segments[[k]] = obs[ini[k]:fin[k]]
    }
    w.old = rep(1, p)
    t.new = rep(1, n)
    p.new = rep(NA, p)
    h = 1
    repeat {
        if (na.miss) {
            for (j in 1:p) {
                i.exist = which(complete.cases(X[, j]))
                w.old[j] = sum(X.old[i.exist, j] * Y.old[i.exist])
            }
            w.new = w.old/sqrt(sum(w.old^2))
            for (i in 1:n) {
                j.exist = which(complete.cases(X[i, ]))
                t.new[i] = sum(X.old[i, j.exist] * w.new[j.exist])
            }
            for (j in 1:p) {
                i.exist = intersect(which(complete.cases(X[, j])), which(complete.cases(t.new)))
                p.new[j] = sum(X.old[i.exist, j] * t.new[i.exist])/sum(t.new[i.exist]^2)
            }
            c.new = t(Y.old) %*% t.new/sum(t.new^2)
            u.new = Y.old/as.vector(c.new)
        }
        if (!na.miss) {
            w.old = t(X.old) %*% Y.old/sum(Y.old^2)
            w.new = w.old/sqrt(sum(w.old^2))
            t.new = X.old %*% w.new                 #X-scores,  t
            p.new = t(X.old) %*% t.new/sum(t.new^2)  
            c.new = t(Y.old) %*% t.new/sum(t.new^2) #Y-weights, c
            u.new = Y.old/as.vector(c.new)          #an updated set of Y-scores
            if (crosval) {
                RSS[h + 1] = sum((Y.old - t.new %*% c.new)^2)
                press = rep(0, 10)
                for (i in 1:10) {
                  aux = segments[[i]]
                  Xy.aux = t(X.old[-aux, ]) %*% Y.old[-aux]
                  wh.si = Xy.aux %*% sqrt(solve(t(Xy.aux) %*% Xy.aux))
                  th.si = X.old[-aux, ] %*% wh.si
                  ch.si = t(Y.old[-aux]) %*% th.si %*% solve(t(th.si) %*% th.si)
                  ch.si = as.vector(ch.si)
                  Yhat.si = ch.si * X.old[aux, ] %*% wh.si
                  press[i] = sum((Y.old[aux] - Yhat.si)^2)
                }
                PRESS[h] = sum(press)
                Q2[h] = 1 - PRESS[h]/RSS[h]
            }
        }
        Y.old = Y.old - (t.new %*% c.new)       #remove the present component from X and Y
        X.old = X.old - (t.new %*% t(p.new))
        Th[, h] = t.new
        Ph[, h] = p.new
        Wh[, h] = w.new
        Uh[, h] = u.new
        ch[h] = c.new
        Hot[, h] = (n/(n - 1)) * t.new^2/(sum(t.new^2)/(n - 1))
        hlim[h] = qf(0.95, h, n - h) * (h * (n^2 - 1))/(n * (n - h))
        if (is.null(comps) && crosval) {
            if (Q2[h] < limq2 || h == nc) break
        } else {
            if (h == nc) break
        }
        h = h + 1
    }
    if (crosval) {
        q2cum = rep(NA, h)
        for (k in 1:h) q2cum[k] = prod(PRESS[1:k])/prod(RSS[1:k])
        Q2cum = 1 - q2cum
        Q2cv = cbind(PRESS[1:h], RSS[1:h], Q2[1:h], rep(limq2, h), Q2cum)
        dimnames(Q2cv) = list(1:h, c("PRESS", "RSS", "Q2", "LimQ2", "Q2cum"))
        if (is.null(comps)) h = h - 1
    }
    if (!crosval) Q2cv = NULL
    Th = Th[, 1:h]
    Ph = Ph[, 1:h]
    Wh = Wh[, 1:h]
    Uh = Uh[, 1:h]
    ch = ch[1:h]
    Ws = Wh %*% solve(t(Ph) %*% Wh)
    Bs = as.vector(Ws %*% ch)
    if (!na.miss) {
        Br = Bs * (rep(apply(Y, 2, sd), p)/apply(X, 2, sd))
        cte = as.vector(colMeans(Y) - Br %*% apply(X, 2, mean))
        y.hat = as.vector(X %*% Br + cte)
        cor.xyt = cor(cbind(Xx, y = Yy), Th)
    } else {
        mu.x <- attributes(Xx)$"scaled:center"
        sd.x <- attributes(Xx)$"scaled:scale"
        X.hat = Th %*% t(Ph) %*% diag(sd.x, p, p) + matrix(rep(mu.x, 
            each = n), n, p)
        Br = Bs * (rep(apply(Y, 2, sd), p)/sd.x)
        cte = as.vector(colMeans(response) - Br %*% mu.x)
        y.hat = as.vector(X.hat %*% Br + cte)
        cor.xyt = matrix(NA, p + 1, h)
        for (j in 1:p) {
            i.exist <- which(complete.cases(X[, j]))
            cor.xyt[j, ] = cor(Xx[i.exist, j], Th[i.exist, ])
        }
        cor.xyt[p + 1, ] = cor(Yy, Th)
    }
    resid = as.vector(Y - y.hat)
    R2 = as.vector(cor(Th, Yy))^2
    R2Xy = t(apply(cor.xyt^2, 1, cumsum))
    T2hot = rbind(hlim[1:h], t(apply(Hot[, 1:h], 1, cumsum)))
    dimnames(Wh) = list(colnames(X), paste0("w", 1:h))
    dimnames(Ws) = list(colnames(X), paste0("w*", 1:h))
    dimnames(Th) = list(rownames(X), paste0("t", 1:h))
    dimnames(Ph) = list(colnames(X),paste0("p", 1:h))
    dimnames(Uh) = list(rownames(Y), paste0("u", 1:h))
    names(ch) = paste0("c", 1:h)
    dimnames(T2hot) = list(c("T2", rownames(X)), paste0("H", 1:h))
    names(Bs) = colnames(X)
    names(Br) = colnames(X)
    names(resid) = rownames(Y)
    names(y.hat) = rownames(Y)
    names(R2) = paste0("t", 1:h)
    colnames(R2Xy) = paste0("t", 1:h)
    dimnames(cor.xyt) = list(c(colnames(X), colnames(Y)), colnames(Th))
    res = list(
      x.scores = Th, x.loads = Ph,
      y.scores = Uh, y.loads = ch,
      cor.xyt = cor.xyt,
      raw.wgs = Wh, mod.wgs = Ws,
      std.coefs = Bs, reg.coefs = c(Intercept = cte, Br),
      R2 = R2,
      R2Xy = R2Xy,
      y.pred = y.hat,
      resid = resid,
      T2 = T2hot,
      Q2 = Q2cv,
      y = response
    )
    class(res) = "plsreg1"
    return(res)
}
## ----------------------------- END PLS --------------------------------
