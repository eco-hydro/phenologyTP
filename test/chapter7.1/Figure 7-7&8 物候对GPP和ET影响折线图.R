source("test/main_pkgs.R")

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

id_veg_010deg <- overlap_id(grid_010.TP_cliped, TP_poly_veg)

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
        foreach(Y = listk(GPP, ET, Ec = l_PML$Ec, Es = l_PML$Es)) %do% {
            get_regional_mean(X, Y, areas)
        }
    } %>% melt_tree(c("metric", "variable"))
    dplot
} %>% melt_list("source")

df = df[region != "热带雨林", ]
df$source %<>% factor(sources[5:7], sources_labels[5:7])
df$metric %<>% factor(c("SOS", "EOS", "LOS"), c("生长季开始时间", "生长季结束时间", "生长季长度") %>% label_tag(tag = FALSE))
df[, x2 := mark_outlier(x), .(source, metric, variable)]
df[, y2 := mark_outlier(y), .(source, metric, variable)]

{
    fontsize <- 15
    theme_set(theme_bw(base_size = fontsize, base_family = "TimesSimSun") + 
        theme(
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(), 
            panel.border = element_rect(size = 0.5)
        ))
}

{    
    devices = c("emf", "pdf")[1]
    show = FALSE
    plot_region_mean(df, "Ec", ylab = expression("植被蒸腾 (mm" ~ a^-1*")"), 
        devices = devices, show = show)
    plot_region_mean(df, "Es", ylab = expression("土壤蒸发 (mm" ~ a^-1*")"),
        devices = devices, show = show)
    plot_region_mean(df, "ET", ylab = expression("蒸发变化 (mm" ~ a^-1*")"),
        devices = devices, show = show)
    plot_region_mean(df, "GPP", ylab = expression("总初级生产力变化 (" * gC ~ m^-2 ~ a^-1 * ")"),
        devices = devices, show = show)
}
