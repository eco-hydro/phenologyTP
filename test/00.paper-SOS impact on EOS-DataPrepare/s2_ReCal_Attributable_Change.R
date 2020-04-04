source("test/main_pkgs.R")

load(file_plsr) 
load(file_trend)

load("data/00basement_TP.rda") 
load(file_pheno_010)
load(file_preseason)

## 2. --------------------------------------------------------------------------
Figure1.2 = FALSE
if (Figure1.2){
  sources <- c("GIMMS", "GIMMS before 2000", "GIMMS after 2001", "MCD12Q2", "SPOT")
  lst_trend <- list(MK = l_mk, LM = l_lm)

  # update attributable change
  res <- foreach(l_trend = lst_trend) %do% {
      foreach(obj = lst_plsr[c(1, 1, 1, 2, 3)], trend = l_trend) %do% {
          ans = attribute_change(obj, trend)
      } %>% set_names(sources)
  }

  df <- melt_tree(res, c("type_trend", "type_source")) %>% melt(c("type_trend", "type_source", "I"))
  # ggplot(df, aes(variable, value)) + geom_boxplot2() + 
  #     facet_grid(source~type_trend)  
}

## 2. Visualization ============================================================
coord <- coordinates(gridclip2_10) %>% set_colnames(c("lon", "lat"))
df %<>% cbind(coord)

df[value >  1, value := 1]
df[value < -1, value := -1]

# p <- ggplot(dplot, aes(lon, lat, fill = value*10)) + 
#     geom_raster() + 
#     facet_grid(variable~source) + 
#     scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdYlGn"))

## Check about meteorological factors' trend
sources = df$source %>% unique() # %>% .[c(1, 3, 2, 4, 5)]
df$source %<>% factor(sources)

test1 = FALSE
if (test1 = TRUE)
{
    varnames <- unique(df$variable)
    lattice.options("panel.levelplot" = "panel.levelplot.raster")
    # dplot$source %<>% as.factor()
    
    brks <- list(
        EOS  = seq(-1, 1, 0.1) %>% c(-Inf, ., Inf),
        Tmin = seq(-1, 1, 0.1) %>% c(-Inf, ., Inf)/2,
        Tmax = seq(-1, 1, 0.1) %>% c(-Inf, ., Inf)/2,
        Prec = seq(-1, 1, 0.1) %>% c(-Inf, ., Inf)/2,
        Srad = seq(-1, 1, 0.1) %>% c(-Inf, ., Inf)/2,
        SOS  = seq(-1, 1, 0.1) %>% c(-Inf, ., Inf)/2
    )
    colors <- list(
        EOS  = .colors$EOS,
        Tmin = .colors$Tavg,
        Tmax = .colors$Tavg,
        # Tavg = .colors$Tavg,
        Prcp = .colors$Prec,
        Srad = .colors$Srad, 
        SOS  = .colors$SOS
    )
    ps <- foreach(varname = varnames, i = icount()) %do% {
        runningId(i)
        settting = opt_trellis_strip
        if (i == 1) settting$layout.heights$top.padding = 0.2
        if (i == 6) settting$layout.heights$bottom.padding = 0.2
        
        strip = ifelse(i == 1, TRUE, FALSE)
        dplot = df[type_trend == "MK" & variable == varname]
        cofun = colorRampPalette(colors[[i]])
        p <- levelplot(value ~ lon * lat | source * variable, dplot, 
                       aspect = 0.75, 
                       at = brks[[i]],
                       xlab = NULL, ylab = NULL,
                       xlim = xlim, ylim = ylim, 
                       scales = list(draw = FALSE, x = FALSE, y = FALSE), 
                       col.regions = cofun(22),
                       colorkey = list(height = 0.8, width = 1.25, fontfamily = "Times"),
                       panel = panel.gridplot, 
                       sp.layout = sp_layout, 
                       strip = list(cex = 2), 
                       par.settings = settting, 
                       as.table = TRUE) %>% 
            useOuterStrips(strip = strip, 
                           strip.left = TRUE)
    }
    margin = 0.18
    p_final <- arrangeGrob(grobs = ps, ncol = 1, heights = c(11.2 + margin, 10, 10, 10, 10, 10 + margin))
    write_fig(p_final, "FigureS2_attributable_changes_all_factors.tif", 10, 8.6)
}
