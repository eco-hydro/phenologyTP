source("test/main_pkgs.R")
# load("data-raw/pheno_TP_010deg (1982-2015).rda")
lst_pheno <- readRDS(file_pheno_full)

# 1. 2001-2014
# 2. 2001-2017
# 3. 1981-2014
# 4. 2000-2018
# 5. 1998-2013
# 6. 1982-2015
sources <- c("MCD12Q2_V5", "MCD12Q2_V6", "VIPpheno_EVI2", 
             "VIPpheno_NDVI", "MOD13C1", "SPOT", "GIMMS3g")

l_time <- list(
  1:14, 
  1:14,  # 2001-2014
  21:34, #
  21:34, # 2001:2014
  2:15, # 2001:2014
  4:16, # 2001:2013
  20:33 # 2001:2014
)
l_pheno <- foreach(l = lst_pheno, i_time = l_time, i = icount()) %do% {
  if (i <= 4) {
    # 2000:2014
    d_pheno <- map(l, ~rowMeans2(.[, i_time], na.rm = TRUE)) %>% as.data.table() %>% cbind(I = 1:nrow(.), .)    
  } else {
    d_pheno <- abind(l, along = 3)[,,i_time] %>% apply_3d() %>% 
      set_colnames(metric_all) %>% as.data.table() %>% cbind(I = 1:nrow(.), .)  
  }
} #%>% melt_list("type_source")
# res$type_source %<>% factor(sources)

# V6的MidGreendown很不合理，时间偏早
## get spring phenological metrics
{
  metrics = c("Greenup" , "DER.pop", "TRS6.eos") # Dormancy
  d_pheno <- foreach(d = l_pheno, i = icount()) %do% {
    if (i <= 2) {
        if (i == 1) names = c("I", "Greenup", "Dormancy")
        if (i == 2) names = c("I", "Greenup", "Dormancy") # MidGreendown
        ans <- d[, .SD, .SDcols = names] %>% set_colnames(c("I", "SOS", "EOS")) %>% 
          plyr::mutate(POP = NA_integer_) %>% .[, .(I, SOS, POP, EOS)]
    } else if (i %in% c(3, 4)) {
        d
    } else {
      names = c("I", metrics)
      d[, .SD, .SDcols = names] %>% set_colnames(c("I", "SOS", "POP", "EOS"))
    }
  } %>% melt_list("type_source")
  d_pheno$type_source %<>% factor(sources)
}

# parameters for levelplot2 in TP
{
    pars = list(
        title = list(x = 77, y = 39, cex = 1.5),
        hist = list(
            origin.x = 77, origin.y = 28, A = 12, by = 0.5, box.width = 0.48, ylab.offset = 3,
            tick = seq(0, 0.3, 0.1)
        )
    )
    stat = list(show = TRUE, name = "均值", loc = c(84.5, 25.7), digit = 1, include.sd = TRUE, FUN = weightedMean)
    # grid <- grid_010.TP_cliped
    # grid <- grid_010.TP_cliped
    SpatialPixel = grid_010.TP_cliped
    labels <- c(expression("MCD12Q2"[V5]), expression("MCD12Q2"[V6]), 
                expression("VIPpheno"[EVI2]), 
                expression("VIPpheno"[NDVI]), 
                "MOD13C1", "SPOT", 
                expression("GIMMS"[3*g])) %>% char2expr()
}

# file <- file_MOD13C1_010
# file <- file_SPOT_010
# d_MOD <- readRDS(file) %>% abind(along = 3)
# r <- apply_3d(d_MOD, 3, rowMeans2) %>% set_colnames(colnames(d_MOD))
# 1. 生长季开始时间
grid <-grid_010.TP_cliped

## 检查peak
Figure4_7 = TRUE
if (Figure4_7) {
  ymax = 10
  brks = .brks$SOS
  cols = get_color("MPL_RdYlGn") %>% rev()
  # cols = RColorBrewer::brewer.pal(11, "RdYlBu")
  p <- levelplot2(SOS ~ s1 + s2 | type_source, 
                  d_pheno, SpatialPixel, 
                  # df.mask[type_trend == "MK" & variable %in% varnames],
                  sp.layout = sp_layout,
                  layout = c(2, 4),
                  ylim = c(25.5, 40.5),
                  xlim = c(73.2, 104.98),
                  colors = cols,
                  stat = stat, pars = pars,
                  brks = brks,
                  strip = TRUE,
                  strip.factors = labels,
                  # panel.titles_full = label_tag(rep("", 6)), 
                  density = 0.5,
                  par.shade = list(lwd = 0.5), 
                  border = "white", border.lwd = 0.01,
                  legend.space = "bottom", 
                  legend.num2factor = TRUE, 
                  par.settings2 = list(strip.background = list(alpha = 0)),
                  par.strip.text = list(cex = 1.5, font = 2, fontfamily = "Times", lineheight = 2),
                  # par.settings = opt_trellis_strip,
                  interpolate = FALSE,
                  aspect = .55) 
  # p %<>% useOuterStrips(strip = strip.custom(factor.levels = labels),
  #                       strip.left = strip.custom(factor.levels = varnames_zh),
  #                       strip.lines = 1.1)
  scale = 1
  write_fig(p, "Figure 4-7 SOS spatial distribution.pdf", 6.5, 9, devices = c("jpg"), show = FALSE) # clear and small file size
}

# 1. 生长季结束时间
Figure4_8 <- TRUE
if (Figure4_8) {
    ymax <- 10
    brks <- .brks$EOS
    cols = get_color("MPL_RdYlGn") #%>% rev()
    p <- levelplot2(EOS ~ s1 + s2 | type_source,
        d_pheno, SpatialPixel,
        # df.mask[type_trend == "MK" & variable %in% varnames],
        sp.layout = sp_layout,
        layout = c(2, 4),
        ylim = c(25.5, 40.5),
        xlim = c(73.2, 104.98),
        colors = cols,
        stat = stat, pars = pars,
        brks = brks,
        strip = TRUE,
        strip.factors = labels,
        # panel.titles_full = label_tag(rep("", 6)),
        density = 0.5,
        par.shade = list(lwd = 0.5),
        border = "white", border.lwd = 0.01,
        legend.space = "bottom",
        legend.num2factor = TRUE,
        par.settings2 = list(strip.background = list(alpha = 0)),
        par.strip.text = list(cex = 1.5, font = 2, fontfamily = "Times", lineheight = 2),
        # par.settings = opt_trellis_strip,
        interpolate = FALSE,
        aspect = .55
    )
    # p %<>% useOuterStrips(strip = strip.custom(factor.levels = labels),
    #                       strip.left = strip.custom(factor.levels = varnames_zh),
    #                       strip.lines = 1.1)
    scale <- 1
    write_fig(p, "Figure 4-8 EOS spatial distribution.pdf", 6.5, 9, devices = c("jpg"), show = FALSE) # clear and small file size
}
