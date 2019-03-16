
MankKendall.Trend <- function(x, brk, addName = T, trim = 1){
    if (trim == 1){
        brk[brk < 5 | brk > 27] <- NA
    }else if (trim == 2){
        brk[brk < 5] <- 5
        brk[brk > 27] <- 27
    }
    #cat(sprintf("Running %dth\n", i))
    info <- numeric(15)*NA
    if (!is.na(brk)){
        x_before <- x[1:brk]
        x_after <- x[(brk+1):length(x)]
        ## 还是需要对空值进行处理
        info <- c(mkTrend(x_before), mkTrend(x_after), mkTrend(x))
    }

    if (addName) 
        names(info) <- c("before.Z", "before.p", "before.Zc", "before.p.adj", "before.SenSlope", 
              "after.Z", "after.p", "after.Zc", "after.p.adj", "after.SenSlope", 
              "Z", "p", "Zc", "padj", "slp")
    info#quickly return
}

MankKendall.Trend_apply <- function(x, psi, conf, sign = NULL, trim = 1){
  n <- nrow(x)
  if (!is.null(sign)) psi[conf > sign] <- NA
  n <- length(psi)
  ## 做线性回归时序列长度保证至少为5
  
  TrendNames <- c("before.Z", "before.p", "before.Zc", "before.p.adj", "before.SenSlope", 
                  "after.Z", "after.p", "after.Zc", "after.p.adj", "after.SenSlope", 
                  "Z", "p", "Zc", "padj", "slp")
  TrendInfo <- data.frame(matrix(NA, nrow = length(psi), ncol = 15, dimnames = 
                                   list(NULL, TrendNames)))
  for (i in 1:n){
    if (mod(i, 100) == 0) 
      cat(sprintf("[%4d]th ------------\n", i))
    brk <- psi[i]
    if (!is.na(brk)){
      x_before <- x[i, 1:brk]
      x_after <- x[i, (brk+1):ncol(x)]
      ## 还是需要对空值进行处理
      TrendInfo[i, 1:10] <- c(mkTrend(x_before), mkTrend(x_after))
    }
    TrendInfo[i, 11:15] <- mkTrend(x[i, ])
  }
  TrendInfo#quickly return

  Id_brk <- !is.na(psi)
  nbrk_slp <- nbrk_z <- brk_slp <- brk_z <- numeric(length(Id_brk))*NA
  nbrk_slp[!Id_brk] <- TrendInfo$slp[!Id_brk]
  nbrk_z[!Id_brk] <- TrendInfo$Z[!Id_brk]
  brk_slp[Id_brk] <- TrendInfo$slp[Id_brk]
  brk_z[Id_brk] <- TrendInfo$Z[Id_brk]
  cbind(TrendInfo, brk_slp, brk_z, nbrk_slp, nbrk_z)#quickly return
}
