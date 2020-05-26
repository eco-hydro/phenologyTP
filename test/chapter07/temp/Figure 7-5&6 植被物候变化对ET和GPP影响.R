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
    l_PML <- map(lst_dynamic, ~.[ind, info$I_y])    

    ET <- abind(l_PML[-1], along = 3) %>% apply_3d(FUN = rowSums2)
    INPUT <- c(list(ET = ET), l_PML[-4])[c(2, 1, 3, 4)]
    res_pcor <- foreach(data = INPUT) %do% {
        corr_pheno(data, SOS, EOS)
    }
}
# chapter 7-1 ------------------------------------------------------------------
lst_pcor <- transpose(temp) %>% map(transpose)
save(lst_pcor, file = "chp7_GPP&ET_pcor.rda")

# load("data-raw/chp7_GPP&ET_pcor.rda")
{
    grid <- grid_010.TP_cliped
    ngrid <- length(grid)
    
    # grid <- grid_010.TP_cliped
    # grid <- grid_010.TP_cliped
    SpatialPixel = grid_010.TP_cliped
}

# figure1 -----------------------------------------------------------------
lst_pcor2 <- map(lst_pcor, tidy_list, ngrid = ngrid)
# df_GPP <- tidy_list(lst_pcor$GPP, ngrid)
# df_ET <- tidy_list(lst_pcor$ET, ngrid)

devices = c("jpg", "pdf")[2]
show = TRUE
plot_pcor_spatial(lst_pcor2$GPP, outfile = "Figure7-5 GPP pcor with phenology.pdf", devices = devices, show = show)
plot_pcor_spatial(lst_pcor2$ET , outfile = "Figure7-5 ET pcor with phenology.pdf", devices = devices, show = show)
plot_pcor_spatial(lst_pcor2$Ec , outfile = "Figure7-5 Ec pcor with phenology.pdf", devices = devices, show = show)
plot_pcor_spatial(lst_pcor2$Es , outfile = "Figure7-5 Es pcor with phenology.pdf", devices = devices, show = show)

# delta_GPP ~ SOS, EOS, LOS, LAI_max

