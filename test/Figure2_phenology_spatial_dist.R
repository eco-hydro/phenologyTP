# Visualization -----------------------------------------------------------
# setwd("..")

source("../phenofit/test/load_pkgs.R")
load("INPUT/phenology_TP_AVHRR_multi-annual.rda")
colnames(df_pheno_avg)[1] <- "row"

{
    source("test/main_TSF.R")
    source("R/panel.barchart.sp.R")
    # source("../phenofit/test/load_pkgs.R")
    source("test/main_spplot.R")
    source("test/main_pkgs.R")
    
    sp_layout <- list("sp.polygons", poly_veg, first = FALSE)
    
    A     = 20
    ntick = 2
    by    = 0.6
        
    # ps <- foreach(d_avg = lst_avg, meth = names(lst_avg)) %do% {
        # gridclip <- fill_grid(gridclip, d_avg)
        gridclip <- fill_grid(gridclip, df_pheno_avg)
        # gridclip@data <- df_pheno_avg[, -1] %>%  as.data.frame()    
        
        p_SOS <- spplot_grid(gridclip, metric_spring[2], #[-c(7, 9)], 
                               panel.title = "(a) SOS", 
                               brks = brks$SOS, colors = colors$SOS)
        p_EOS <- spplot_grid(gridclip,  metric_autumn[2], #[-c(6, 7, 9)], 
                               panel.title = "(b) EOS",
                               brks = brks$EOS, colors = colors$EOS)
        
        g <- arrangeGrob(p_SOS, p_EOS, nrow = 1)
    # }
    # g <- arrangeGrob(grobs = ps, padding = unit(0, "line"))
    
    # p_EOS
    write_fig(g, "Figure2_phenology_dist.pdf", 12, 3)
    # write_fig(p_SOS, "phenofit_TP_SOS2.pdf", 11, 8)
    # write_fig(p_EOS, "phenofit_TP_EOS2.pdf", 11, 8)
  
    # brks <- brks_SOS
    # spplot(gridclip, 1, at = brks, drop.unused.levels = FALSE,
    #        colorkey=list(space = 'right',
    #                      # at = check_brks(brks),
    #                      tri.upper = 0.1, tri.lower = 0.1, 
    #                      tck = -4,
    #                      # labels=list(at = check_brks(brks), labels = brks,
    #                      #             cex=1.2, fontface='bold', fontfamily='times'),
    #                      height = 1)
    # )
}
