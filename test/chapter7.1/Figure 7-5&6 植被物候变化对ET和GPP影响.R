source("test/main_pkgs.R")

load(file_PML)
load("data-raw/pheno_trend.2000_2015.rda")
## 1. 准备一个0.1deg的高程文件
d_elv <- readGDAL("E:/github/hydro/VICtools/inst/database/ELEV_srtm_010deg_china.tif") %>% as_SpatialPixelsDataFrame()
{
    grid <- grid_010.TP
    dem <- raster::extract(raster(d_elv), grid_010.TP)
    grid@data <- data.table(dem)
    dem <- grid
    spplot(dem)
    # use_data(dem, overwrite = TRUE)
    # write_tiff(dem, "data-raw/ELEV_srtm_010deg_TP.tif")
}
# %% ---------------------------------------------------------------------------
lst_pheno <- readRDS(file_pheno)
years_gpp <- 2003:2017

grid_010.TP@data <- data.frame(id = 1:length(grid_010.TP))
ind <- raster::extract(raster(grid_010.TP), grid_010.TP_cliped)

temp <- foreach(l = lst_pheno, i = icount()) %do% {
    info <- match2(l$year, years_gpp)
    l_pheno <- map(l[c(1,3)] %>% rm_empty, ~.[, info$I_x])

    SOS <- l_pheno$SOS
    EOS <- l_pheno$EOS
    # LOS <- EOS - SOS
    # summary(as.numeric(LOS))
    # LOS[LOS <= 0] <- NA
    # summary(as.numeric(LOS))
    l_PML <- map(df_dynamic, ~.[ind, info$I_y])    

    pcor_gpp <- corr_pheno(l_PML$GPP, SOS, EOS)
    ET <- abind(l_PML[-1], along = 3) %>% apply_3d(FUN = rowSums2)
    pcor_ET <- corr_pheno(ET, SOS, EOS)
    list(GPP = pcor_gpp, ET = pcor_ET)
}
# chapter 7-1 ------------------------------------------------------------------

lst_pcor <- transpose(temp) %>% map(transpose)
save(lst_pcor, file = "chp7_GPP&ET_pcor.rda")

{
    grid <- grid_010.TP_cliped
    ngrid <- length(grid)

    stat = list(show = TRUE, name = "均值", loc = c(84.5, 25.7), digit = 1, include.sd = TRUE, FUN = weightedMean)
    stat_sign = list(loc1 = c(78, 41.5), loc2 = c(78, 41.5 - 2))
    pars = list(
        title = list(x = 74, y = 41, cex = 1.5),
        hist = list(
            origin.x = 77, origin.y = 28, A = 12, by = 0.5, box.width = 0.48, ylab.offset = 3,
            tick = seq(0, 0.3, 0.1)
        )
    )
    # grid <- grid_010.TP_cliped
    # grid <- grid_010.TP_cliped
    SpatialPixel = grid_010.TP_cliped
}

# figure1 -----------------------------------------------------------------
df_GPP <- tidy_list(lst_pcor$GPP, ngrid)
df_ET <- tidy_list(lst_pcor$ET, ngrid)

Figure7_5 = TRUE
Figure7_6 = TRUE

if (Figure7_5) {
    df = df_GPP
    # brks = .brks$SOS
    brks = c(0.2, 0.6, 1) %>% c(-rev(.), 0, .)
    # cols = get_color("MPL_RdYlGn") %>% rev()
    cols = RColorBrewer::brewer.pal(11, "RdYlBu") #%>% rev()
    p <- levelplot2(value ~ s1 + s2 | type_source * variable, 
                    df, SpatialPixel, 
                    # df,
                    # df.mask[type_trend == "MK" & variable %in% varnames],
                    sp.layout = sp_layout,
                    # layout = c(2, 4),
                    ylim = c(25.5, 43),
                    xlim = c(73.2, 104.98),
                    colors = cols,
                    stat = stat, pars = pars,
                    stat_sign = stat_sign, 
                    brks = brks,
                    strip = TRUE,
                    # strip.factors = sources_labels,
                    panel.titles_full = label_tag(rep("", 6)),
                    density = 0.5,
                    par.shade = list(lwd = 0.5),
                    border = "white", border.lwd = 0.01,
                    legend.space = "right",
                    legend.num2factor = TRUE,
                    par.settings2 = list(strip.background = list(alpha = 0)),
                    par.strip.text = list(cex = 1.5, font = 2, fontfamily = "Times", lineheight = 2),
                    # par.settings = opt_trellis_strip,
                    interpolate = FALSE,
                    aspect = .7) +
        theme_lattice(
            # key.margin = c(0, 1, 0, 0),
            plot.margin = c(0.2, 2.5, -1.5, 0.2))
    p %<>% useOuterStrips(
        strip = strip.custom(factor.levels = sources_labels[5:7]), # %>% label_tag()
        # strip.left = strip.custom(factor.levels = varnames_zh),
        strip.lines = 1.1)
    scale = 1
    write_fig(p, "Figure7-5 GPP pcor with phenology.pdf", 10, 4.7, devices = c("jpg", "pdf"), show = TRUE) # clear and small file size
}

# figure2 ----------------------------------------------------------------------
if (Figure7_6) {
    df = df_ET
    # brks = .brks$SOS
    brks <- c(0.2, 0.6, 1) %>% c(-rev(.), 0, .)
    # cols = get_color("MPL_RdYlGn") %>% rev()
    cols <- RColorBrewer::brewer.pal(11, "RdYlBu") # %>% rev()
    p <- levelplot2(value ~ s1 + s2 | type_source * variable,
        df, SpatialPixel,
        # df,
        # df.mask[type_trend == "MK" & variable %in% varnames],
        sp.layout = sp_layout,
        # layout = c(2, 4),
        ylim = c(25.5, 43),
        xlim = c(73.2, 104.98),
        colors = cols,
        stat = stat, pars = pars,
        stat_sign = stat_sign,
        brks = brks,
        strip = TRUE,
        # strip.factors = sources_labels,
        panel.titles_full = label_tag(rep("", 6)),
        density = 0.5,
        par.shade = list(lwd = 0.5),
        border = "white", border.lwd = 0.01,
        legend.space = "right",
        legend.num2factor = TRUE,
        par.settings2 = list(strip.background = list(alpha = 0)),
        par.strip.text = list(cex = 1.5, font = 2, fontfamily = "Times", lineheight = 2),
        # par.settings = opt_trellis_strip,
        interpolate = FALSE,
        aspect = .7
    ) +
        theme_lattice(
            # key.margin = c(0, 1, 0, 0),
            plot.margin = c(0.2, 2.5, -1.5, 0.2)
        )
    p %<>% useOuterStrips(
        strip = strip.custom(factor.levels = sources_labels[5:7]), # %>% label_tag()
        # strip.left = strip.custom(factor.levels = varnames_zh),
        strip.lines = 1.1
    )
    scale <- 1
    write_fig(p, "Figure7-6 ET pcor with phenology.pdf", 10, 4.7, devices = c("jpg", "pdf"), show = TRUE) # clear and small file size
}
