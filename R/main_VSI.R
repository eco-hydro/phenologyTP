
#########################################################
# TRIM THE ANOMALIES FOR A VARIABLE
cutAnomaly <- function(res, alpha = 0.005, Iplot = TRUE) {
  # Cut the anomolies
  anom <- quantile(res, c(alpha, 1- alpha), na.rm = TRUE)
  
  if (Iplot){
    plot(res)
    abline(h = anom, col = "red")
  }
  
  res[res < anom[1]] <- anom[1]
  res[res > anom[2]] <- anom[2]
  return(res)
}

# Range function to standardise between 0 and 100
range01 <- function(x){
  minVal <- min(x, na.rm = T)
  maxVal <- max(x, na.rm = T)
  (x - minVal)/(maxVal - minVal)*100#quickly return
}

# Function for estimating variance anomalies
anomCalcNew <- function(data, Iplot = TRUE){
  var <- as.character(data[1, "variable"])#figure title
  lm_fit <- lm(sd ~ mean + I(mean^2), data)
  res <- residuals(lm_fit)
  
  #NA VALUES effect res length and order
  # Id <- as.numeric(attr(res, "names"))
  Id <- match(attr(res, "names"), rownames(data))
  RES <- rep(NA_real_, nrow(data)); RES[Id] <- res
  res <- RES
  
  if (Iplot){
    p <- ggplot(data, aes(x = mean, y = sd)) + geom_point(alpha = 1/8) + 
      stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = T, level = 0.95) + ggtitle(var)
    print(p)
    # plot(sd~mean, data)
    # # plot(x[smp], y[smp], pch = 16, xlab = "Mean", ylab = "Sd")
    # xPred <- seq(min(data$mean, na.rm = TRUE), max(data$mean, na.rm = TRUE), length.out = 1000)
    # yPred <- predict(lm_fit, newdata = data.frame(mean = xPred))
    # 
    # lines(xPred, yPred)
  }
  anomNew <- cutAnomaly(res, alpha = 0.01, Iplot = FALSE) %>% range01
  return(anomNew)
}

# vsiAVHRR_year <- function(NDVI.avhrr, metes, i){
#   NDVI <- NDVI.avhrr[i, ] %>% matrix(nrow = 12)
#   NDVI_t1 <- matrix(c(NA, NDVI.avhrr[i, -1]), nrow = 12)
#   mete <- llply(metes, function(x) matrix(x[i, ], nrow = 12))
  
#   dataIn <- c(list(NDVI = NDVI, t1 = NDVI_t1), mete)
#   xlist <- lapply(1:12, function(i) lapply(dataIn, `[`, i = i, ) %>% {na.omit(do.call(cbind.data.frame, .))}) %>% set_names(1:12)
  
#   ## mask month in which NDVI value is low than minimum threshold or mean air temperature below than 0.
#   #  NDVI_monthAvg <- apply(NDVI, 1, mean)
#   #  Tavg_monthAvg <- apply(mete$Tavg, 1, mean)
#   #  monthMask <- which((NDVI_monthAvg < NDVI_min) |  (Tavg_monthAvg < 0))
  
#   vsi <- llply(xlist, vsi_month)
#   vars <- names(vsi[[1]])
#   vsi.month <- llply(vars, function(var) ldply(vsi, `[[`, var, .id = NULL)) %>% set_names(vars)
#   return(vsi.month)
# }

pcor_year <- function(x, t1 = FALSE) plyr::ldply(x, pcor_month, .id = NULL, t1 = t1)
# add tryCatch to pcor function
pcor_month <- function(x, t1 = FALSE) {
  if (!t1) x <- x[, -2]#t2 is the second column
  nvar <- ncol(x)
  tryCatch(suppressWarnings(ppcor::pcor(na.omit(x)))$estimate[1, 2:nvar], 
    error = function(e) setNames(rep(NA, nvar - 1), colnames(x)[2:nvar]))
}

# if include t1, set t1 = TRUE
vsi_year <- function(x, t1 = FALSE) plyr::llply(x, vsi_month, t1)


#' vsi_month
#' @param full monthly data.frame ([NDVI, t1, Prec, Srad, Tmin, Tavg, Tmax]) input data
vsi_month <- function(full, t1 = FALSE){
  ## detrend function
  # det.func<-function(x) {
  #   x <- t(x) 
  #   x <- x - rowMeans(x, na.rm=T) 
  #   t(x)
  # }
  det.func <- function(x) x - mean(x, na.rm = T)
  
  #modified 2017-03-24, include Tavg variable
  if (!t1) full <- full[, -2] #trim t1, c(2, 6)
  nvar <- ncol(full)
  r2 <- pVal <- NA
  # pvalue <- rep(NA, 4)
  varCoef <- upCI <- lowCI <- matrix(NA_real_, ncol = nvar - 1, dimnames = list(NULL, colnames(full)[-1]))
  sdDetVec <- meanVec <- set_names(rep(NA, nvar), colnames(full))

  if (nrow(full) > 0){
    # full <- xlist[[1]]
    rownames(full) <- 1:nrow(full)
    # full = data.frame(NDVI = NDVI[month, ], t1 = NDVI_t1[month, ], lapply(mete, `[`, month, ))
    # Calculate detrended matrices
    # full %<>% do.call(cbind.data.frame, .)
    ## debug fullZ rownames 
    fullDet <- lapply(full, det.func) %>% do.call(cbind.data.frame, .)#subtract multi-annual means
    fullZ <- lapply(full, scale) %>% do.call(cbind.data.frame, .)#standard for mean and variance
    
    sdDetVec <- unlist(lapply(fullDet, sd, na.rm = T))
    meanVec <- unlist(lapply(full, mean, na.rm = T))
    
    varPCA = na.omit(fullZ[, -1])
    pca.model <- tryCatch(princomp(varPCA), error = function(e) NULL)
    # suppressMessages(pca.model <- try(princomp(varPCA)))#, na.action=na.omit
    
    if (!is.null(pca.model)){
      # estimate the loadings of each of the variables on the 3 different PC axes
      nPC <- 4
      PCAloadings <- pca.model$loadings[, 1:nPC]
      # original row order changed due to na.omit action
      pcTableMatch <- match(rownames(pca.model$scores), rownames(full))#due to delete NA values
      # Find the scores for PC1, PC2, PC3, PC4
      PCs <- matrix(NA, nrow(full), nPC, dimnames = list(NULL, paste0("PC", 1:nPC)))
      PCs[pcTableMatch, ] <- pca.model$scores[, 1:nPC] 
      
      # Regress the PC scores agacldt the evi data
      varTable <- data.frame(resp = fullZ$NDVI, PCs) %>% na.omit()
      
      if(nrow(varTable)>=6){  
        formula <- paste("PC", 1:nPC, collapse = " + ", sep = "")
        eval(parse(text = sprintf("model <- lm(resp ~ %s, varTable)", formula)))
        # model <- lm(resp ~ PC1 + PC2 + PC3 + PC4, data = varTable)  
        
        tryCatch({
          # extract the cofficients of the PC regression 
          regCoef <- model$coefficients[-1]#delete interception coefficients
          
          # Find the rsquared and pvalue of the PC regression
          lm_summary <- summary(model)
          # pvalue <- lm_summary$coefficients[-1, 4]
          r2 <- lm_summary$adj.r.squared#r.squared
          
          fstat <- lm_summary$fstatistic   
          pVal <- pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE)
          cInf <- confint(model, c("PC1", "PC2", "PC3", "PC4"), level = 0.9)
          # if a pc axis is signicant at p = 0.1 it will tramsform these back to the original coefficients by mulitplying the loadings of that axis by the regression coefficients
          sigCoef <- which(lm_summary$coefficients[-1,4] < 0.05)
          PCAloadings.sig <- PCAloadings[, sigCoef, drop = FALSE]
          
          if(length(sigCoef) > 0){
            varCoef <- PCAloadings.sig %*% regCoef[sigCoef] %>% t
            # Find the upper and lower confidence in the same way
            upCI  <- PCAloadings.sig %*% cInf[sigCoef, 2] %>% t
            lowCI <- PCAloadings.sig %*% cInf[sigCoef, 1] %>% t
            # vsi <<- listk(varCoef, upCI, lowCI, r2, pVal)
          } # closes the sig if statement 
        }, 
        error = function(e) e)
      }
      # print(str(vsi))
    }
  }
  vsi <- listk(varCoef, upCI, lowCI, r2, pVal, sdDetVec, meanVec)
  return(vsi)
}

get_Sens <- function(Mean, Sd, Coef){
  # X = mean.list[[i]]
  # Y = sd.list[[i]]
  # Id <- 1:nrow(X)
  # Id_sel <- Id#sample(Id, 2000)
  
  x <- Mean %>% melt("Id", value.name = "mean")
  y <- Sd %>% melt("Id", value.name = "sd")
  df <- join(x, y, by = c("Id", "variable"))[, -1]
  # Id_deld <- which(!complete.cases(df[, c("mean", "sd")]))
  # if (length(Id_del) > 0) df <- df[-Id_deld, ]
  var_list <- split(df, df$variable)#varlist

  # ggplot(df, aes(x = mean, y = sd)) + geom_point(alpha = 1/8) + facet_wrap(~variable, scale = "free") + 
  #   stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = T)
  #   stat_smooth(method = "loess")
  
  Anom <- llply(var_list, anomCalcNew, Iplot = FALSE) %>% list.cbind()
  NDVI <- Anom$NDVI
  sens <- llply(Anom[-1], function(x) log((NDVI + 1)/(x + 1)) %>% range01())
  
  # M <- Reduce(`+`, sens)/length(sens)
  # M <- rowMeans(do.call(cbind, sens), na.rm = TRUE)
  
  Coef %<>% lapply(range01)
  sens.w <- mapply(`*`, sens, Coef) %>% {llply(data.frame(.), range01)}
  # sens.w <- mapply(`*`, sens, coef)
  # sensTol.w <- rowMeans(sens.w, na.rm = TRUE) %>% range01()#final vegetation sensitivity index
  # vsi <- data.frame(llply(data.frame(sens.w), range01) %>% do.call(cbind.data.frame, .), 
  #                 vsi = sensTol.w)
  return(sens.w)
}

# meteVars <- c("Tmax", "Tavg", "Tmin", "Prec", "Srad")
# meteVars <- c("Tmax", "Tmin", "Prec", "Srad")

# meteVars should be declare in main script
## prepare plot data
VSI_data <- function(VSI, var = "varCoef"){
  ## try to visual PCALM coef directly
  
  # varCoef <- lapply(VSI, `[[`, var)
  # varCoef <- llply(VSI, function(x) ldply(x, `[[`, "varCoef", .id = NULL)[, meteVars])
  # varCoef.list <- llply(1:12, function(i) ldply(varCoef, function(x) x[i,], .id = NULL), .progress = "text") %>% set_names(1:12)
  monthId <- 4:10 %>% {set_names(as.list(.), .)}
  varCoef.list <- llply(monthId, function(i) ldply(VSI, function(x) x[[i]][[var]][, meteVars], .id = NULL), .progress = "text")
  sd.list   <- llply(monthId, function(i) ldply(VSI,   function(x) x[[i]]$sdDetVec[meteVars], .id = "Id"), .progress = "text")
  mean.list <- llply(monthId, function(i) ldply(VSI, function(x) x[[i]]$meanVec[meteVars], .id = "Id"), .progress = "text")

  # mapply(mean.list, sd.list, varCoef.list)
  sensw <- foreach(Mean = mean.list, Sd = sd.list, Coef = varCoef.list, i = icount()) %do%{
    print(i)
    get_Sens(Mean, Sd, Coef)
  } %>% set_names(names(monthId)) %>% list.cbind()

  # pVal <- ldply(VSI, function(x) as.numeric(as.matrix(ldply(x, `[[`, "pVal", .id = NULL))), .progress = "text", .id = NULL)
  coef <- list.cbind(varCoef.list) %>% set_colnames(paste0("M", colnames(.)))
  ## dominated factors
  opt <- llply(varCoef.list, function(x) {
    # Rc <- get_critical(0.1, 32 - 6)
    # x[x < Rc & x > -Rc] <- NA

    opt <- apply(x, 1, function(x) which.max(abs(x))) 
    opt[sapply(opt, length) == 0] <- NA
    opt <- factor(names(unlist(opt)), meteVars)
    return(opt)
  }, .progress = "text") %>% list.cbind %>% set_colnames(paste0("M", colnames(.)))
  listk(coef, sensw, opt)
}

VSI_show_dominate <- function(x, filename = "dominate.png"){
  nvar <- length(levels(x[[1]]))
  colors <- c("firebrick1", "orange3", "chartreuse4", "blue", "purple")[1:nvar]

  gridClip@data <- x
  zcol <- colnames(x)
  A = 8; by=0.9; ntick = 2
  p <- spplot(gridClip, col.regions = colors, 
              sp.layout = list(sp_cluster), as.table = T, layout = c(3, 2), 
              xlim = xlim, ylim = ylim, strip = F, 
         panel = function (x, y, z, subscripts, ...,  sp.layout) 
         {
           sppanel(list(sp.layout), panel.number(), first = TRUE)
           panel.levelplot.raster(x, y, z, subscripts, ..., interpolate = F)
           # panel.contourplot(x, y, z, subscripts, ..., contour = TRUE, labels = F)
           sppanel(list(sp.layout), panel.number(), first = FALSE)
           
           i <- panel.number()
           panel.text(76, 40.4, zcol[i], #english name: New_names[i])
                      fontfamily = "Times", cex = 1.3, font = 2, adj = 0)
           # panel.text(76.6, 40.8 +heights[i], metricsEN[i], fontfamily = "Times", cex = 1.2, font = 2, adj = 0)
           panel.addbarchart(z, subscripts, colors, showCluster = F, A = A, by = 0.9, ntick = ntick, ...)
         }, 
         par.settings = list(axis.line = list(col = "transparent")))

  CairoPNG(filename, 10, 4, dpi = 300, pointsize = 12, units = "in")
  # spplot(gridClip, as.table = T, layout = c(5, 12))
  print(p)
  dev.off()
}

VSI_show_coef <- function(
  x, 
  brks, 
  filename = "coef.png",
  sign = FALSE, 
  signVal = brks[1]
){
  zcol <- colnames(x)
  nmonths <- 6
  nvar <- ncol(x)/nmonths
  A = 8; by=0.9; ntick = 2
  breaks <- round(brks, 3) %>% {c(-rev(.), 0, .)}

  if (sign) {
    x[abs(x) < signVal] <- NA
  }

  ncol <- length(breaks) - 1
  cols <- colorRampPalette(c("firebrick1","orange3", "darkgoldenrod2", "grey90",
                             brewer.pal(9, "YlGnBu")[c(4, 6, 7)], "green4"))(ncol)#,colors()[504]


  gridClip@data <- llply(x, cut, breaks = breaks, include.lowest = T) %>% do.call(cbind.data.frame,.)
  p <- spplot(gridClip, zcol = zcol, col.regions = cols, 
              # scales = list(draw=T), 
              as.table = T, layout = c(nvar, nmonths),
              # sp.layout = list(sp_cluster, sp_points, sp_text),
              sp.layout = list(sp_cluster),
              xlim = xlim, ylim = ylim, strip = F, 
              # colorkey = list(labels = list(labels = levels(cut(1, brks)), at = seq_along(brks),
              colorkey = list(labels = list(labels = breaks[-c(1, length(breaks))], at = seq_along(breaks[-(1:2)]) + 0.5,
                                            cex = 1.3, fontfamily = "Times", fontface = 2), 
                              axis.line = list(col = 'black'), 
                              height = 0.9, 
                              space = "bottom"), 
              panel = function (x, y, z, subscripts, ...,  sp.layout) 
              {
                sppanel(list(sp.layout), panel.number(), first = TRUE)
                panel.levelplot.raster(x, y, z, subscripts, ..., interpolate = T)
                # panel.contourplot(x, y, z, subscripts, ..., contour = TRUE, labels = F)
                sppanel(list(sp.layout), panel.number(), first = FALSE)
                
                i <- panel.number()
                panel.text(76, 40.4, paste0(zcol[i]), #english name: New_names[i])
                           fontfamily = "Times", cex = 1.3, font = 2, adj = 0)#"(",letters[i], ") ", 
                # panel.text(76.6, 40.8 +heights[i], metricsEN[i], fontfamily = "Times", cex = 1.2, font = 2, adj = 0)
                panel.addbarchart(z, subscripts, cols, showCluster = F, A = A, by = 0.9, ntick = ntick, origin.x = 77, ...)
              },
              par.settings = list(axis.line = list(col = "transparent")))

  CairoPNG(filename, heigh = 12, width = 15, dpi = 300, pointsize = 12, units = "in")
  # spplot(gridClip, as.table = T, layout = c(5, 12))
  print(p)
  dev.off()
  # width = 900; height = 520; ratio = 3.5
  # if (saveFIG) CairoPNG(filename = file, width*ratio, height*ratio, dpi = 250, pointsize = 12)
  # print(p)
  # grid::grid.text(legendName, x=unit(0.945, "npc"), y=unit(0.936, "npc"), 
  #                 gp=grid::gpar(fontfamily = "Times", fontsize=14, fontface = "bold"))
  # if (saveFIG) dev.off()
}
