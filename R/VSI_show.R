## critical values, with years = 32 and n = 32



# ps <- VSI_show(x, brks_z)
# ps.add <- llply(1:ncol(x), function(i) VSI_show(x[, i, drop = F], brks_z, sign = TRUE))

test <- function(fill = "transparent", col = "red") grid.rect(gp = gpar(fill = fill, col = col))


VSI_show <- function(x,
                     brks,
                     filename = "coef.png",
                     saveFIG = FALSE,
                     sign = FALSE,
                     signVal = brks[1],
                     ...
){
  dots <- list(...)
  A = 8; by=0.9; ntick = 2
  breaks <- round(brks, 3) %>% {c(-rev(.), 0, .)}
  colors <- get_color(breaks)# %>% list2env(environment())
  cols <- colors$cols
  colorkey <- colors$colorkey
  # show_col(cols)#debug use
  
  zcol <- colnames(x)
  if ("zcol" %in% names(dots)) zcol <- dots$zcol
  
  nmonths <- 7
  nvar <- ncol(x)/nmonths
  # IF ONLY DISPLAY significant part
  if (sign) {
    x[abs(x) < signVal] <- NA
    colorkey <- FALSE
  }else{
    ylim[2] <- 45
  }
  
  x[, zcol] <- llply(x[, zcol, drop=FALSE], cut, breaks = breaks, include.lowest = T) %>% do.call(cbind.data.frame,.)
  gridClip@data <- x
  params <- list(
    obj = gridClip,
    zcol = zcol,
    col.regions = cols,
    # scales = list(draw=T),
    as.table = T,
    # layout = c(nvar, nmonths),
    # sp.layout = list(sp_cluster, sp_points, sp_text),
    sp.layout = list(sp_cluster),
    xlim = xlim, ylim = ylim, 
    strip = strip.custom(factor.levels = paste0("(", letters[1:ncol(x)], ") ", colnames(x[, zcol, drop = FALSE]))), 
    # colorkey = list(labels = list(labels = levels(cut(1, brks)), at = seq_along(brks),
    colorkey = colorkey, 
    interpolate = T,
    par.strip.text = list(fontfamily = "Times", cex = 1.1, font = 2), 
    panel = function (x, y, z, subscripts, ...,  sp.layout) 
    {
      sppanel(list(sp.layout), panel.number(), first = TRUE)
      panel.levelplot.raster(x, y, z, subscripts, ...)
      # panel.contourplot(x, y, z, subscripts, ..., contour = TRUE, labels = F)
      sppanel(list(sp.layout), panel.number(), first = FALSE)
      #not significant subplot
      if (!sign){
        i <- panel.number()
        # panel.text(76, 40.4, paste0(zcol[i]), #english name: New_names[i])
                 # fontfamily = "Times", cex = 1.3, font = 2, adj = 0)#"(",letters[i], ") ", 
        # panel.text(76.6, 40.8 +heights[i], metricsEN[i], fontfamily = "Times", cex = 1.2, font = 2, adj = 0)
        panel.addbarchart(z, subscripts, cols, showCluster = F, A = A, by = 0.9, ntick = ntick, origin.x = 76, ...)
      }
    })
  
  Id_dup <- which(names(params) %in% names(dots))
  params <- c(dots, if (length(Id_dup) > 0) params[-Id_dup] else params)
  
  if (sign) {
    params$`par.settings` = list(axis.line = list(col = "transparent"))
  }else{
    params$`par.settings` = list(layout.heights = list(strip=1.2))
  }
  # par.settings = list(axis.line = list(col = "transparent")))
  # params %<>% list.remove("panel")
  # params$colorkey <- FALSE
  # params$layout <- c(2, 2)
  b <- addNAemptyRowsCols(params$obj) %>% as("data.frame")
  
  p <- do.call(spplot, params)
   
  # if (saveFIG) CairoPNG(filename, 10, 4, dpi = 300, pointsize = 12, units = "in")
  # print(p)
  # if (saveFIG) dev.off()
  p
}

# for significant subplot
makeViewPorts <- function(i, bbox){
  vpStack(
    viewport(
      name = "ozlay",
      layout = grid.layout(1, 1, widths = diff(bbox[1,]), heights = diff(bbox[2,]), respect = T)
    ),
    viewport(
      name = "ozvp", 
      layout.pos.row = 1, layout.pos.col = 1,
      xscale = bbox[1,], yscale = bbox[2,], 
      clip = TRUE
    )
  )
}

# layout = c(2, 2): according to parameter layout to find panels viewport
VSI_show.sub <- function(x, brks, layout = c(2, 2)){
  ps <- VSI_show(x, brks, layout = layout)
  #get sign according to brks[1], it needs to adjust
  ps.add <- llply(1:ncol(x), function(i) VSI_show(x[, i, drop = F], brks, sign = TRUE))

  print(ps)
  for (i in 1:ncol(x)){
    # downViewport("plot_01.panel.1.1.off.vp")
    ncol <- ps$layout[1]
    row <- (i - 1) %/% ncol + 1
    col <- (i - 1) %% ncol + 1
    vpname <- sprintf("plot_01.panel.%d.%d.vp", col, row) %T>% {cat(sprintf("[%d]: %s\n", i, .))}
    
    downViewport(vpname)
    # grid.rect(gp = gpar(fill = "transparent", col = "red"))
    # downViewport("ROOT::plot_01.panel.1.1.off.vp::plot_01.border.panel.1.1")
    pd <- grid.grabExpr(print(ps.add[[i]]))
    gTree(name="add", 
          gp = NULL, vp= viewport(x = -0.01, y = 1.11, just = c("left", "top"), 
                                  width = 0.55, height = 0.55), 
          childrenvp = makeViewPorts(i, bbox), 
          children = gList(pd), 
          cl="ozGrob") %>% grid.draw()
    upViewport(0)
  }
}

VSI_show.subList <- function(x, brks){
  # ps <- VSI_show(x, brks, layout = layout)
  ps <- llply(1:ncol(x), function(i) VSI_show(x[, i, drop = F], brks[[i]]))
  #get sign according to brks[1], it needs to adjust
  ps.add <- llply(1:ncol(x), function(i) VSI_show(x[, i, drop = F], brks[[i]], sign = TRUE, strip = FALSE))

  layout <- ps$layout
  if (is.null(layout)){
    ncol <- 2
    nrow <- (ncol(x) - 1) %/% ncol + 1
  }else{
    ncol <- layout[1]
    nrow <- layout[2]
  }
  
  pushViewport(viewport(layout = grid.layout(nrow, ncol)))  
  # grid.rect(gp = gpar(fill = "transparent", col = "red"))
  # upViewport()

  # pushViewport(viewport(layout.pos.col = 2))
  # grid.rect(gp = gpar(fill = "transparent", col = "black"))
  # upViewport()

  for (i in 1:ncol(x)){
    #debug use
    # pushViewport(viewport(layout = grid.layout(nrow, ncol)))  
    # downViewport("plot_01.panel.1.1.off.vp")
    row <- (i - 1) %/% ncol + 1
    col <- (i - 1) %% ncol + 1

    pushViewport(viewport(layout.pos.col = col, layout.pos.row = row))
    # vpname <- sprintf("plot_01.panel.%d.%d.vp", col, row) %T>% {cat(sprintf("[%d]: %s\n", i, .))}
    # downViewport(vpname)
    
    # downViewport("ROOT::plot_01.panel.1.1.off.vp::plot_01.border.panel.1.1")
    # p <- grid.grabExpr(print(ps[[i]], newpage = F), wrap = T)
    print(ps[[i]], newpage = F)
    vps <- grid.ls(grob = F, viewports = T, print = F)
    vps_panel <- str_subset(unique(vp$name), ".+panel\\.1\\.1\\.vp") %>% sort(decreasing = T)

    # vpname <- sprintf("plot_%02d.panel.1.1.vp", i) %T>% {cat(sprintf("[%d]: %s\n", i, .))}
    vpname <- vps_panel[1] %T>% {cat(sprintf("[%d]: %s\n", i, .))}
    downViewport(vpname)
    # grid.rect(gp = gpar(fill = "transparent", col = "red"))

    pd <- grid.grabExpr(print(ps.add[[i]]))
    gTree(name="add", 
          gp = NULL, vp= viewport(x = -0.01, y = 1.11, just = c("left", "top"), 
                                  width = 0.55, height = 0.55), 
          childrenvp = makeViewPorts(i, bbox), 
          children = gList(pd), 
          cl="ozGrob") %>% grid.draw()
    upViewport(3)
    current.viewport()
  }
}