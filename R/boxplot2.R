#' boxplot2
#' 
#' x: variable
#' y: varname
#' 
#' @export
boxplot2 <- function(d, varname = "perc", ylab, include.EOS = FALSE) {
    if (!include.EOS) {
        d <- d[variable != "EOS"]
    }
    eval(parse(text = sprintf('d_avg <- d[, boxplot_5p(%s), .(variable)]', varname)))
    
    A = with(d_avg, max(ymax, na.rm = TRUE) - min(ymin, na.rm = TRUE))
    A.adj <- A*0.04
    ggplot(d, aes_string("variable", varname, fill = "variable")) + 
        # geom_histogram(stat='identity') + 
        stat_boxplot(geom ='errorbar', width = 0.5) +
        geom_boxplot2(outlier.stroke = 0) + 
        geom_text(data = d_avg, aes(y = middle + A.adj, label = sprintf("%.1f", middle)), 
            color = "black", size =5) + 
        # geom_point(shape = 4, size = 4) + 
        labs(y = ylab, x = NULL) + 
        theme(
            panel.grid = element_blank(),
            axis.title.y.left = element_text(margin = margin(r = 10)),
            legend.position = "none")
}
