# source("test/main_pkgs.R")
suppressMessages({
    library(lubridate)
    library(data.table)
    
    library(grid)
    library(lattice)
    library(latticeExtra)
    library(latticeGrob) # should behind of latticeExtra
    library(Cairo)
    library(scales)
    # library(ggpmisc)

    library(plyr)
    library(tidyverse)
    library(magrittr)

    # library(parallel)
    library(foreach)
    library(iterators)

    library(maptools)
    library(sf)
    library(segmented)
    library(pls)
    # library(ropls)
    library(RColorBrewer)

    # library(plsdepot)#already rewrite its PLSREG1 function
    library(matrixStats)
    library(ppcor)

    library(data.table)
    library(matrixStats)
    library(abind)
    library(glue)
    
    # Myself packages 
    library(Ipaper)
    library(rcolors)
    # library(phenology)
    library(phenofit)
    library(CMIP5tools)
    library(rPML)

    library(sp)
    # library(oce)
    library(sp2)
    library(raster)
    library(rgdal)
    #library(plsdepot)

    library(rPML)
})

file_PML <- "data-raw/PMLV2_TP_010deg_v016.rda"

range_global <- c(-180, 180, -60, 90)
grid_global  <- get_grid(range_global, cellsize = 0.1, type = "vec")

prj_TP               <- path.mnt("n:/Research/phenology/phenologyTP/")
file_pheno_012       <- paste0(prj_TP, "OUTPUT/phenology_TP_AVHRR_phenofit.rda")

file_pheno_010       <- paste0(prj_TP, "OUTPUT/phenology_TP_AVHRR_phenofit_010deg.rda")
file_pheno_010_3s    <- paste0(prj_TP, "OUTPUT/phenology_TP_phenology_010deg_3s.rda")
file_pheno_010_3s_V2 <- paste0(prj_TP, "OUTPUT/phenology_TP_phenology_010deg_3s_V2.rda")

file_preseason       <- paste0(prj_TP, "OUTPUT/TP_010deg_preseason2.rda")
file_plsr            <- paste0(prj_TP, "OUTPUT/TP_010deg_PLSR_SOS and Non-SOS_V2.rda")
file_plsr_mk         <- paste0(prj_TP, "OUTPUT/TP_010deg_PLSR_SOS and Non-SOS_(slope_mk).rda")

file_trend           <- paste0(prj_TP, "INPUT/preseason_trend.rda")

file_SPOT_010        <- path.mnt("F:/SciData/pheno_TP (SPOT&MODIS)/phenofit_SPOT_TP_010deg_v2.RDS")
file_MOD13C1_010     <- path.mnt("F:/SciData/pheno_TP (SPOT&MODIS)/phenofit_MOD13C1_TP_010deg.RDS")
file_AVHRR_010       <- "data-raw/phenology_AVHRR_010deg_TP_cliped (1982-2015).rds"
file_pheno_full      <- "data-raw/phenology_6types_010deg_TP_cliped (1982-2015).RDS"
file_pheno           <- "data-raw/phenology_6types_010deg_TP_cliped 2metrics (1982-2015).RDS"

# source('test/main_vis.R')
# panel.grid = element_blank(), 
theme_set( theme_bw(base_size = 14) + 
    theme(
        panel.grid.minor = element_blank()
        ))
    # theme(panel.grid.minor = element_blank()) 

titles_a <- c(expression(bold("(a) "*GIMMS[3*g]*" SOS")), 
    expression(bold("(b) "*GIMMS[3*g]*" EOS")))
titles_b <- c("MCD12Q2", "VIP_Pheno") %>% rep(each = 2) %>% paste(c("SOS", "EOS"), sep = " ") %>% 
    sprintf("(%s) %s", letters[3:6], .)

A     = 20
ntick = 2
by    = 0.6

select_metric <- function(mat){
    mat[, c("TRS2.sos", "TRS6.eos")] %>% set_colnames(c("SOS", "EOS")) %>% as.data.table()
}

namesSatellites = c(
    expression(bold("(a) GIMMS"[3*g])), 
    expression(bold("(b) MODIS")), 
    expression(bold("(c) SPOT")))
