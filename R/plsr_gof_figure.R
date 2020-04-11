plsr_gof_figure <- function(df, outfile = "Figure2_PLSR_model_performance.png", FUN = weightedMean){
    labels = c(
        expression(bold("GIMMS"[3*g])), 
        expression(bold("MODIS")), 
        expression(bold("SPOT")))
    
    names      = c("RMSE", "MAE", quote(R^2))
    indexes_eq = c(expression(bold("RMSE")), expression(bold("MAE")), expression(bold(R^2)))
    
    cols = c("firebrick1","orange3", "darkgoldenrod2", "grey90",
             brewer.pal(9, "YlGnBu")[c(4, 6, 7)]) %>% rev()

    pars = list(title = list(x=77, y=39.6, cex=1.2), 
                hist = list(origin.x=76.8, origin.y=27.6, A=12, by = 0.6, ylab.offset = 3.2, tick = seq(0, 0.3, 0.1)))
    brks0 = c(3, 4, 5, 7, 10, 15)
    
    ps = foreach(indexName = indexes, i = icount()) %do% {
        brks = switch(i, brks0, brks0,
            seq(0.1, 0.7, 0.1)) %>% c(-Inf, ., Inf)

        digit = switch(i, 1, 1, 2)
        stat = list(show = TRUE, name=names[[i]], loc = c(83, 25.7), digit = digit, include.sd = TRUE, FUN = FUN)
        
        colors = switch(i,
                        cols, cols, 
                        RColorBrewer::brewer.pal(11, "RdYlGn"))
        
        right = 1.5
        plot.margin = switch(i,
                             c(1, right, 0, 0.5), 
                             c(0, right, 0, 0.5), 
                             c(0, right, 1, 0.5)
        )
        
        d = df[index == indexName]
        strip = ifelse(i == 1, TRUE, FALSE)
        unit  = ifelse(i == 3, "", "days")
        padding = ifelse(i == 1, 0.2, 0) # legend top padding
        
        # browser()
        p <- levelplot2(
            value ~ s1+s2| variable + index, d, SpatialPixel, 
            stat = stat, 
            brks = brks,
            legend.num2factor = TRUE, 
            NO_begin = (i-1)*3+1,
            pars = pars,
            colors = colors,
            ylim = c(25.5, 40.5),
            xlim = c(72.9, 104.98),
            # unit = unit, 
            # unit.adj = 5,
            colorkey = list(key.padding = c(padding, 0), height = 0.98, width = 1.5, unit = ""),
            par.strip.text = list(cex = 1.5, font = 2, fontfamily = "Times", lineheight = 2), 
            sp.layout = sp_layout,
            padding = unit(3.5, "line"),
            interpolate = FALSE,
            aspect = .6
        ) 
        
        # browser()
        if (i == 1) strip = strip.custom(factor.levels = labels)
        p2 = useOuterStrips(p, strip = strip, 
                            strip.left = strip.custom(factor.levels = indexes_eq[i])) + 
            theme_lattice(
                plot.margin = plot.margin,
                key.margin  = c(0, 1.5, 0, 0),
                axis.line   = list(col = "black"),
                layout.heights = list(
                    key.top = 0,
                    xlab.key.padding  = 0,
                    key.sub.padding   = 0,
                    key.axis.padding = 0
                ))
        p2
    }
    height.padding = 0.486
    height.strip = 1.75
    g = arrangeGrob(grobs = ps, ncol = 1,
                    heights = c(10 + height.padding + height.strip, 10, 10 + height.padding))
    write_fig(g, outfile, 9.8, 5.6)
}


func_Figure2.diff <- function(df, lst_brks, outfile = "Figure2_PLSR_model_performance.png", FUN = weightedMean){
    labels = c(
        expression(bold("GIMMS"[3*g])), 
        expression(bold("MODIS")), 
        expression(bold("SPOT")))
    
    names      = c("RMSE", "MAE", quote(R^2))
    indexes_eq = c(expression(bold("RMSE")), expression(bold("MAE")), expression(bold(R^2)))
    
    cols = c("firebrick1","orange3", "darkgoldenrod2", "grey90",
             brewer.pal(9, "YlGnBu")[c(4, 6, 7)]) %>% rev()


    pars = list(title = list(x=77, y=39.6, cex=1.2), 
                hist = list(origin.x=76.8, origin.y=27.6, A=12, by = 0.6, ylab.offset = 3.2, tick = seq(0, 0.3, 0.1)))
    
    ps = foreach(indexName = indexes, i = icount()) %do% {
        digit = switch(i, 1, 1, 2)
        x.stat = switch(i, 83, 83, 84.5)
        stat = list(show = TRUE, name=names[[i]], loc = c(x.stat, 25.7), digit = digit, include.sd = TRUE, FUN = FUN)
        pars$hist$origin.y = switch(i, 27.6, 27.6, 28)

        colors = switch(i,
                        cols, cols, 
                        RColorBrewer::brewer.pal(11, "RdYlGn"))
        
        right = 3
        left  = switch(i, 0.2, 0.2, 0.4)
        plot.margin = switch(i,
                             c(1, right, 0, left), 
                             c(0, right, 0, left), 
                             c(0, right, 1, left)
        )
        
        d = df[index == indexName]
        strip = ifelse(i == 1, TRUE, FALSE)
        unit  = ifelse(i == 3, "", "days")
        padding = ifelse(i == 1, 0.2, 0) # legend top padding
        
        p <- levelplot2(
            value ~ s1+s2| variable + index, d, SpatialPixel, 
            stat = stat, 
            brks = lst_brks[[i]],
            legend.num2factor = TRUE, 
            NO_begin = (i-1)*3+1,
            pars = pars,
            colors = colors,
            ylim = c(25.5, 40.5),
            xlim = c(72.9, 104.98),
            # unit = unit, 
            # unit.adj = 5,
            colorkey = list(key.padding = c(padding, 0), height = 0.98, width = 1.5, unit = ""),
            par.strip.text = list(cex = 1.5, font = 2, fontfamily = "Times", lineheight = 2), 
            sp.layout = sp_layout,
            padding = unit(3.5, "line"),
            interpolate = FALSE,
            aspect = .6
        ) 
        
        if (i == 1) strip = strip.custom(factor.levels = labels)
        p2 = useOuterStrips(p, strip = strip, 
                            strip.left = strip.custom(factor.levels = indexes_eq[i])) + 
            theme_lattice(
                plot.margin = plot.margin, 
                key.margin  = c(0, 1.5, 0, 0), 
                axis.line   = list(col = "black"), 
                layout.heights = list(
                    key.top = 0,
                    xlab.key.padding  = 0,
                    key.sub.padding   = 0,
                    key.axis.padding = 0
                ))
        p2
    }
    height.padding = 0.486
    height.strip = 1.75
    g = arrangeGrob(grobs = ps, ncol = 1,
                    heights = c(10 + height.padding + height.strip, 10, 10 + height.padding))
    write_fig(g, outfile, 9.8, 5.6)
}
