## Fig5 Trend before and after TP: 
# Trend_spplot.grid(gridClip2, Trend.before,New_names, at.before)
Trend_spplot.grid <- function(gridClip, x, zcol, at, cols = NULL){
  if (!is.data.frame(x)) x <- as.data.frame(x)
  gridClip@data <- x
  nat <- length(at);
  if (is.null(cols)) cols <- col_fun(length(at) - 1) #col_fun:globalenv variable
  New_names <- zcol
  spplot(gridClip2, New_names, as.table = T, col.regions = cols, 
         sp.layout = sp.cluster, strip = F, 
         xlim = bbox(cluster)[1, ], ylim =  bbox(cluster)[2, ], 
         layout = c(4, 5), 
         colorkey = list(space = "bottom", height  = 0.8, title = "doys/decades", 
                         labels = list(labels = at[2:(nat - 1)], 
                                       at = 2:(nat - 1) - 0.5,
                                       cex = 1.1, fontfamily = "Times", fontface = 1),
                         axis.line = list(col = 'black')),
         #legend=list(list(fun=grid::textGrob("days\n(decades)", x=1.09))), 
         panel = function (x, y, z, subscripts, ...,  sp.layout) 
         {
           sppanel(list(sp.layout), panel.number(), first = TRUE)
           ## 转折不显著的设置成灰色
           Id_na <- is.na(z); zmark <- z; zmark[Id_na] <- 0; zmark[!Id_na] <- NA
           panel.levelplot.raster(x, y, zmark, subscripts, col.regions = "grey80", interpolate = T)
           panel.levelplot.raster(x, y, z, subscripts, ..., interpolate = T)

           i <- panel.number()
           panel.text(76.6, 39.3, paste0("(",letters[i], ") ", New_names[i]), fontfamily = "Times", cex = 1.2, adj = 0)
           sppanel(list(sp.layout), panel.number(), first = FALSE)
           panel.addbarchart(z, subscripts, cols, showCluster = F)
         }, par.settings = list(axis.line = list(col = "transparent")))
  ## add colorkey legend title
  # trellis.focus("legend", side="bottom", clipp.off=TRUE, highlight=FALSE)
  # grid::grid.text(expression(paste("(", day/decade, ")")), 0.907, 0.5, hjust=0, vjust=0, 
  #                 gp=gpar(fontfamily = "Times", cex = 1.2))
  # trellis.unfocus()
}
