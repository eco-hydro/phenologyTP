library(Ipaper)
library(Rcmip5)
library(phenofit)
library(sp)
library(magrittr)
library(phenology)
library(lattice)
library(foreach)
library(iterators)
library(tidyverse)
library(lubridate)
library(oce)

# 1. 计算积温生长季长度，并绘制空间图
# 2. 为了核对物候指标提取的效果

load("data/00basement_TP.rda")
# source("../phenofit/test/load_pkgs.R")
## 1. 计算积温生长季长度 --------------------------------------------------------
system.time({
    killCluster()
    InitCluster(8)

    indir <- "G:/SciData/中国数据/Data_forcing_01dy_010deg"
    lst_pheno <- foreach(year = 1981:2015, j = icount()) %do% {
        # 1. read Tavg
        file <- sprintf("%s/temp_ITPCAS-CMFD_V0106_B-01_01dy_010deg_%d01-%d12.nc", indir, year, year)
        r <- ncread_cmip5(file, "temp", range = range, ntime = -1, 
                      shiftLon = FALSE, delta = 0, offset = -273.15)
        # 2. Thermal growing season
        mat <- r$value %>% array_3dTo2d(I_grid)

        pheno <- foreach(x = iter(mat, "row"), i = icount()) %dopar% {
            phenofit::runningId(i, 1000, prefix = year)
            phenology::PhenoThermal(x, T_SOS = 0, T_EOS = 0, nday_SOS = 5, nday_EOS = 5, IsPlot = FALSE)
            # title(main = i)
        }
        do.call(rbind, pheno) %>% as.data.table()
    } 
    df_pheno <- lst_pheno %>% map(~cbind(row = 1:nrow(.), .)) %>% set_names(1981:2015) %>% melt_list("year")
    df_pheno_avg <- df_pheno[, lapply(.SD, mean, na.rm = T), .(row), .SDcols = colnames(df_pheno)[2:4]]
    save(lst_pheno, df_pheno, df_pheno_avg, file = "PhenoThermal_0_0.rda")
    # save(lst_pheno, file = )
})

# grid@data <- data.frame(Tavg = as.numeric(mat %>% fliplr())); spplot(grid)

# grid@data <- data.frame(Tavg = as.numeric(mat %>% fliplr())); spplot(grid)
# mat <- r$value[,,1]

load("OUTPUT/PhenoThermal_0_0.rda")
gridclip_10@data <- df_pheno_avg[, 2:3] %>% as.data.frame()
# gridclip@data <- r$value %>% array_3dTo2d(I_grid) %>% .[, 1:4] %>% as.data.frame()
# spplot(gridclip, 1:4)

{
    source("test/main_TSF.R")
    source("R/panel.barchart.sp.R")
    # source("../phenofit/test/load_pkgs.R")
    source("test/main_spplot.R")
    source("test/main_pkgs.R")
    
    sp_layout <- list("sp.polygons", poly_veg, first = FALSE)
    
    A = 20
    ntick = 2
    
    brks_SOS  <- c(-Inf, seq(80, 160, 10), Inf)
    # brks_SOS  <- c(-Inf, seq(100, 150, 10), Inf)
    brks_EOS <- c(-Inf, seq(250, 310, 5)+10, Inf)
    # ps <- foreach(d_avg = lst_avg, meth = names(lst_avg)) %do% {
        # gridclip <- fill_grid(gridclip, d_avg)
    # gridclip <- fill_grid(gridclip, df_pheno_avg)
    # gridclip@data <- df_pheno_avg[, -1] %>%  as.data.frame()    
    
    p_SOS <- spplot_grid(gridclip_10,  "SOS", 
                           panel.title = NULL, 
                           brks = brks_SOS, colors = colors$SOS)
    p_EOS <- spplot_grid(gridclip_10,  "EOS", 
                           panel.title = NULL,
                           brks = brks_EOS, colors = colors$EOS, 
                           by = 0.6)
    g <- arrangeGrob(p_SOS, p_EOS, nrow = 1)
    write_fig(g, "Figs1_thermal_growing season.pdf", 12, 3)
}
