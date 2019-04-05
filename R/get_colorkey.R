#' @import RColorBrewer
#' @export
get_colorkey <- function(breaks, space = "bottom", lgd.title = NULL, is_factor = FALSE){
    ncolor <- length(breaks) - 1
    cols <- colorRampPalette(c("firebrick1","orange3", "darkgoldenrod2", "grey90",
                               brewer.pal(9, "YlGnBu")[c(4, 6, 7)], "green4"))(ncolor)#,colors()[504]
    #prepare for spplot
    colorkey <- list(
        title = lgd.title,
        labels = list(cex = 1.3, fontfamily = "Times", fontface = 2),
        axis.line = list(col = 'black'),
        rect = list(col = "black", lwd = 0.4), 
        # tri.upper = TRUE,  tri.lower = TRUE, 
        height = 1, space = space, tck = 1
    )

    if (is_factor) {
        colorkey$labels$at <- seq_along(breaks[-(1:2)]) + 0.5
        colorkey$labels$labels <- breaks[-c(1, length(breaks))]
    }
    list(cols = cols, colorkey = colorkey)#return
}
