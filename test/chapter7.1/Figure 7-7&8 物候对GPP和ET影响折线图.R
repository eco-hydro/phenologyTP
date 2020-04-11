source("test/main_pkgs.R")
library(ggpmisc)
load(file_PML)
load("data-raw/pheno_trend.2000_2015.rda")
## 1. 准备一个0.1deg的高程文件
# d_elv <- readGDAL("E:/github/hydro/VICtools/inst/database/ELEV_srtm_010deg_china.tif") %>% as_SpatialPixelsDataFrame()
# {
#     grid <- grid_010.TP
#     dem <- raster::extract(raster(d_elv), grid_010.TP)
#     grid@data <- data.table(dem)
#     dem <- grid
#     spplot(dem)
#     # use_data(dem, overwrite = TRUE)
#     # write_tiff(dem, "data-raw/ELEV_srtm_010deg_TP.tif")
# }
# %% ---------------------------------------------------------------------------
lst_pheno <- readRDS(file_pheno)
years_gpp <- 2003:2017

grid_010.TP@data <- data.frame(id = 1:length(grid_010.TP))
inds <- raster::extract(raster(grid_010.TP), grid_010.TP_cliped)
areas <- values(raster::area(raster(grid_010.TP_cliped)))[grid_010.TP_cliped$id]

df <- foreach(l = lst_pheno[5:7], i = icount()) %do% {
    info <- match2(l$year, years_gpp)
    l_pheno <- map(l[c(1,3)] %>% rm_empty, ~.[, info$I_x])

    SOS <- l_pheno$SOS %>% clamp(lims = c(0  , 300), fill.na = TRUE)
    EOS <- l_pheno$EOS %>% clamp(lims = c(100, 365), fill.na = TRUE)
    LOS <- (EOS - SOS) %>% clamp(lims = c(0, 300), fill.na = TRUE)
    
    l_PML <- map(df_dynamic, ~.[inds, info$I_y])    
    GPP <- l_PML$GPP
    ET  <- abind(l_PML[-1], along = 3) %>% apply_3d(FUN = rowSums2)
    
    dplot <- foreach(X = listk(SOS, EOS, LOS)) %do% {
        foreach(Y = listk(GPP, ET)) %do% {
            get_regional_mean(X, Y, areas)
        }
    } %>% melt_tree(c("metric", "variable"))
    dplot
} %>% melt_list("source")

df$source %<>% factor(sources[5:7], sources_labels[5:7])
df$metric %<>% factor(c("SOS", "EOS", "LOS"), c("SOS", "EOS", "LOS") %>% label_tag(tag = FALSE))
{
    theme_set( theme_bw(base_size = 16, base_family = "TimesSimSun") + 
                   theme(
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(), 
                       panel.border = element_rect(size = 0.5)
                   ))
    p <- ggplot(df[variable == "ET"], aes(x, y)) + geom_point(aes(shape = region, color = region, size = size)) + 
        geom_smooth(method = "lm") + 
        # stat_cor(aes(label = paste(..adj.rr.label.., ..adj.rr.label..))) +
        # stat_regline_equation(label.y.npc = 0.9) +
        stat_fit_tidy(method = "lm",
                      label.x = "left", label.y = 0.02,
                      geom = "label_npc", label.size = NA,
                      method.args = list(formula = y ~ x), parse = TRUE, 
                      mapping = aes(label = sprintf('Slope~"="~%.2f ~ mm ~ a^-1 ~ d^-1',
                                                    stat(x_estimate),
                                                    stat(x_p.value)))) + 
        stat_fit_glance(method = "lm",
                        # label.y = "bottom",
                        label.y = 0.15,
                        geom = "label_npc", label.size = NA,
                        method.args = list(formula = y ~ x),
                        mapping = aes(label = sprintf('italic(p)~"="~%.3f', stat(p.value))), parse = TRUE) + 
                        # mapping = aes(label = sprintf('italic(R^2)~"="~%.3f*","~italic(p)~"="~%.3f', 
                        #                               stat(r.squared), stat(p.value))), parse = TRUE) +
        scale_shape_manual(values = 0:18) + 
        scale_size_manual(values = seq(1, 10, 0.7)) + 
        labs(size = expression("面积 ("*10^3~km*")"), 
             shape = "植被分区", color = "植被分区", fill  = "植被分区",
             x = "植被物候变化 (天)", y = "蒸发变化 (mm/a)") + 
        facet_grid(metric~source, scales = "free", labeller = label_parsed) + 
        theme(strip.text = element_text(family = "Times"))
    write_fig(tag_facet(p, size = 5, vjust = 2), "Figure7-8 ET ~ phenology metrics.jpg", 10, 6.5)
}

## GPP -------------------------------------------------------------------------
{
    p <- ggplot(df[variable == "GPP"], aes(x, y)) + 
        geom_point(aes(shape = region, color = region, size = size)) +
        geom_smooth(method = "lm") +
        # stat_cor(aes(label = paste(..adj.rr.label.., ..adj.rr.label..))) +
        # stat_regline_equation(label.y.npc = 0.9) +
        stat_fit_tidy(
            method = "lm",
            label.x = "left", label.y = 0.02,
            geom = "label_npc", label.size = NA,
            method.args = list(formula = y ~ x), parse = TRUE,
            mapping = aes(label = sprintf(
                'Slope~"="~%.2f ~ gC ~ m^-2 ~ a^-1 ~ d^-1',
                stat(x_estimate),
                stat(x_p.value)
            ))
        ) +
        stat_fit_glance(
            method = "lm",
            # label.y = "bottom",
            label.y = 0.15,
            geom = "label_npc", label.size = NA,
            method.args = list(formula = y ~ x),
            mapping = aes(label = sprintf('italic(p)~"="~%.3f', stat(p.value))), parse = TRUE
        ) +
        # mapping = aes(label = sprintf('italic(R^2)~"="~%.3f*","~italic(p)~"="~%.3f',
        #                               stat(r.squared), stat(p.value))), parse = TRUE) +
        scale_shape_manual(values = 0:18) +
        scale_size_manual(values = seq(1, 10, 0.7)) +
        labs(
            size = expression("面积 (" * 10^3 ~ km * ")"),
            shape = "植被分区", color = "植被分区", fill = "植被分区",
            x = "植被物候变化 (天)", y = expression("总初级生产力GPP变化 ("*gC~m^-2~a^-1*")")
        ) +
        facet_grid(metric ~ source, scales = "free", labeller = label_parsed) +
        theme(strip.text = element_text(family = "Times"))
    write_fig(tag_facet(p, size = 5, vjust = 2), "Figure7-7 GPP ~ phenology metrics.jpg", 10, 6.5)
}

# ggplot(df, aes(x= new_price, y= carat, color = cut)) +
#     geom_point(alpha = 0.3) +
#     facet_wrap(~clarity, scales = "free_y") +
#     geom_smooth(method = "lm", formula = formula, se = F) +
