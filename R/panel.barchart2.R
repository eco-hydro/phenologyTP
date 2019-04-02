windowsFonts(Times = windowsFont("Times New Roman"), 
             Arial = windowsFont("Arial"))

# TP vegetation cluster 
pos <- data.frame(x = c(99, 92.5, 86.5, 98,   81,   102.6,  100,  86), 
                  y = c(29, 33,   31,   33.2, 34.1, 32.8, 36.7, 27))
pos$name <- letters[1:nrow(pos)]
# plot(sp_cluster[[2]], axes = T)
# points(pos$x, pos$y, pch = 3, col = "red"); text(pos$x, pos$y, pos$name)
# plot(seq(75, 105, length.out = 100), seq(20, 40, length.out = 100), type = "n"); text(pos$x, pos$y, pos$name)


#' get factor frequency
#' 
#' @examples
#' get_perc.factor(factor(c(1, 2, 4), 1:4)
get_perc.factor <- function(z){
    z <- z[!is.na(z)] # rm NAN value
    d <- table(z) %>% as.data.frame()
    d$perc <- d$Freq/length(z)
    d
}


#' panel.barchart2
#' 
#' new panel function for lattice, plot barchart in the bottomleft corner of subplot
#'
#' @param  {[type]}    perc      [description]
#' @param  {Number}    origin.x  [description]
#' @param  {Number}    origin.y  [description]
#' @param  {Number}    tck       [description]
#' @param  {Number}    dx        [description]
#' @param  {Number}    A         [description]
#' @param  {Number}    by        [description]
#' @param  {[type]}    col       Colors
#' @param  {[type]}    box.width [description]
#' @param  {Number}    ntick     [description]
#' @param  {Number}    text.cex  [description]
#' @param  {...[type]}           [description]
#' @return {[type]}              [description]
#' 
#' @export
panel.barchart2 <- function(z, subscripts, origin.x = 76, origin.y = 26.5, tck = 0.2, 
    A = 15, by = 0.6, box.width = by*0.85, 
    axis.x.text = TRUE, axis.x.text.angle = 90,
    col, 
    fontfamily = "Times", border = "transparent", ntick = 2, 
    ylab.offset = 2, 
    text.cex = 1, ...)
{
    dots <- list(...)
    col  <- dots$col.regions
    at   <- dots$at

    z <- z[subscripts]
    if (!is.null(at) & !is.factor(z)) {
        z <- cut(z, dots$at) %>% as.numeric()
    }
    perc <- get_perc.factor(z)$perc

    # tck <- 0.2#tick length
    ypos <- perc*A + origin.y 
    xpos <- seq(origin.x + by, by = by, length.out = length(perc))

    panel.barchart(x = xpos, y = ypos, horizontal = F, origin = origin.y, 
                 reference = F, col = col, box.width = box.width, border = border, ...)

    ymax <- ceiling(max(perc)*10)/10
    # ymax <- round(max(perc), 1)
    if (ymax <= 0.1) {
        ntick <- 1
    } else if (ymax <= 0.2){
        ntick <- 2
    }

    tick <- pretty(c(0, ymax), ntick);
    if (ymax >= 0.5 && ymax <= 0.6) tick <- c(0, 0.3, 0.6)

    tick_ypos <- tick*A + origin.y
    tick_xpos <- c(xpos - 0.5*by, max(xpos) + 0.5*by) 

    # avoid overlap
    delta_x = pmax(tck/3 - (by - box.width)/2 , 0)
    tick_xpos[1] <- tick_xpos[1]  - delta_x

    # axis and arrow
    panel.arrows(x0 = tick_xpos[1], y0 = origin.y, 
        c(tick_xpos[1], max(xpos) + tck*4),
        c(ymax*1.2*A + origin.y, origin.y), 
        col.line = 'black', type = 'closed', length = 0.05, col = 'black', fill = 'black', 
        identifier = "axis.arrow")
    # yaxis.tick
    panel.segments(
        x0 = tick_xpos[1], 
        x1 = tick_xpos[1] - tck,
        y0 = tick_ypos, 
        y1 = tick_ypos, identifier = "yaxis.tick"
    )
    # yaxis.text
    panel.text(x = tick_xpos[1]-tck-0.25*by, y = tick_ypos, tick,
        fontfamily = fontfamily, 
        cex = text.cex, adj = 1, font = 2, identifier = "yaxis.text")

    # xaxis.tick.minor
    I <- seq(2, length(tick_xpos)-1, 2)
    panel.segments(
        x0 = tick_xpos[I], 
        x1 = tick_xpos[I],
        y0 = origin.y, 
        y1 = origin.y - tck/2, identifier = "xaxis.tick.minor"
    )
    # xaxis.tick.major
    I <- seq(2, length(tick_xpos)-1, 4)
    panel.segments(
        x0 = tick_xpos[I], 
        x1 = tick_xpos[I],
        y0 = origin.y, 
        y1 = origin.y - tck, identifier = "xaxis.tick.major"
    )

    if (axis.x.text){
        adj <- c(0.5, 1)
        if (axis.x.text.angle == 90) adj <- c(1, 0.5)
        panel.text(x = tick_xpos[I], y = origin.y - tck*1.2, dots$brks[I],
            fontfamily = fontfamily, srt = axis.x.text.angle, 
            cex = text.cex, adj = adj, font = 2, identifier = "xaxis.text")    
    }
    
    panel.text(tick_xpos[1] - ylab.offset, origin.y - tck, "Friction (%)", 
        srt = 90, font = 2, adj = c(0, 0.5), fontfamily = fontfamily, cex = text.cex, 
        identifier = "ylab.text")
}


#' add barchart of every cluster
panel.addbarchart_old <- function(z, subscripts, cols.avg, showCluster = T, ...){
    z <- z[subscripts]
    Id_remain <- which(!is.na(z))
    z <- z[Id_remain]

    # origin.x <- 76; origin.y <- 26.5; tck = 0.2; dx = 0.4; by = 0.6; A = 15; box.width = 0.5
  #   if (showCluster){
  #       ## for every single cluster region
  #       info <- aggregate(factor(z[Id_remain]), list(group = Id_cluster[Id_remain]), summary)[-7, ]
  #       perc <- apply(info$x, 1, function(x) x/sum(x))
  #       for(j in 1:8){
  #           yposi <- perc[, j]*6 + pos[j, 2]
  #           xposi <- seq(pos[j, 1], by = 0.5, length.out = length(yposi))
  #           panel.addaxis(perc[, j], origin.x = pos[j, 1], origin.y = pos[j, 2], dx = 0.3, 
  #                       by = 0.5, box.width = 0.4, 
  #                       col = cols.avg, ntick = 2, border = "grey40", A = 5,
  #                       lwd = 0.5, text.cex = 1)
  #       }
  # }
  
  # divide into grows
  #20161123 modify data_in <- summary(factor(z)); perc <- data_in/sum(data_in);
  data_in <- sapply(1:max(z), function(x) length(which(z == x)))
  perc <- data_in/sum(data_in)
  ## TP summary infomation
  panel.addaxis(perc, col = cols.avg, border = "transparent", ...)
}


cols.fun <- function(n){
  ncol <- n - 1
  cols <- colorRampPalette(c("firebrick1","orange3", "darkgoldenrod2", "grey90", 
                             RColorBrewer::brewer.pal(9, "YlGnBu")[c(4, 6, 7)]))(ncol)#,colors()[504]
  cols <- cols[-5]
  cols[ncol] <- "green4"
  cols <- rev(cols)
  return(cols)
}

# panel.EOS_spatial <- function (x, y, z, subscripts, interpolate, ...,  sp.layout) 
# {
#    # print(str(listk(x, y, z, subscripts, ...)))#debug code
#    sppanel(list(sp.layout), panel.number(), first = TRUE)
#    panel.levelplot.raster(x, y, z, subscripts, ..., interpolate = interpolate)
#    # panel.contourplot(x, y, z, subscripts, ..., contour = TRUE, labels = F)
#    sppanel(list(sp.layout), panel.number(), first = FALSE)
   
#    i <- panel.number()
#    panel.text(76, 40.4, paste0("(",letters[i], ") ", zcol[i]), #english name: New_names[i])
#               fontfamily = "Times", cex = 1.3, font = 2, adj = 0)
#    panel.addbarchart(z, subscripts, cols, showCluster = showCluster, A = A, by = 0.9, ntick = ntick, ...)
# }
EOS_TP.spatial <- function(x, zcol = colnames(x), 
  brks, cols = cols.fun(length(brks)), layout = c(4, 4), 
  legendName = "DOY", saveFIG = TRUE, file, 
  showCluster = F, interpolate = T, 
  # panel = panel.EOS_spatial, 
  A = 8, by=0.9, ntick = 2, ...){

  x    <- x[, zcol]
  zcol <- colnames(x)
  colnames(x) <- paste0("v", 1:ncol(x))
  
  gridClip@data <- llply(x, cut, breaks = brks, include.lowest = T) %>% do.call(cbind.data.frame,.)

  p <- spplot(gridClip, col.regions = cols, 
         # scales = list(draw=T), 
         # checkEmptyRC = T, #addNAemptyRowsCols, has no effects on the results, since it add NA values at tail.
         as.table = T, layout = layout,
         sp.layout = list(sp_cluster, sp_points, sp_text),
         xlim = xlim, ylim = ylim, strip = F, 
         # colorkey = list(labels = list(labels = levels(cut(1, brks)), at = seq_along(brks),
         colorkey = list(labels = list(labels = brks[-c(1, length(brks))], at = seq_along(brks[-(1:2)]) + 0.5,
                                       cex = 1.3, fontfamily = "Times", fontface = 2), 
                         axis.line = list(col = 'black'), 
                         height = 0.9), 
         panel = function (x, y, z, subscripts, ...,  sp.layout) 
        {
           # print(str(listk(x, y, z, subscripts, ...)))#debug code
           sppanel(list(sp.layout), panel.number(), first = TRUE)
           panel.levelplot.raster(x, y, z, subscripts, ..., interpolate = interpolate)
           # panel.contourplot(x, y, z, subscripts, ..., contour = TRUE, labels = F)
           sppanel(list(sp.layout), panel.number(), first = FALSE)
           
           i <- panel.number()
           panel.text(76, 40.4, paste0("(",letters[i], ") ", zcol[i]), #english name: New_names[i])
                      fontfamily = "Times", cex = 1.3, font = 2, adj = 0)
           panel.addbarchart(z, subscripts, cols, showCluster = showCluster, A = A, by = 0.9, ntick = ntick, ...)
        },
         par.settings = list(axis.line = list(col = "transparent")))

  width = 900; height = 520; ratio = 3.5
  if (saveFIG) CairoPNG(filename = file, width*ratio, height*ratio, dpi = 250, pointsize = 12)
  ifelse(saveFIG, print(p), return(p))
  # grid::grid.text(legendName, x=unit(0.945, "npc"), y=unit(0.936, "npc"), 
  #           gp=grid::gpar(fontfamily = "Times", fontsize=14, fontface = "bold"))
  if (saveFIG) dev.off()
}
# grid::grid.text(legendName, x=unit(0.945, "npc"), y=unit(0.936, "npc"), 
#                 gp=grid::gpar(fontfamily = "Times", fontsize=14, fontface = "bold"))

