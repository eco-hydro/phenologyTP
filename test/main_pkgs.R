# source("test/main_pkgs.R")
suppressMessages({
    library(lubridate)
    library(data.table)
    # library(grid)

    library(lattice)
    library(latticeExtra)
    library(Cairo)
    library(scales)

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
    library(ropls)
    library(RColorBrewer)

    # library(plsdepot)#already rewrite its PLSREG1 function
    library(matrixStats)
    library(ppcor)

    library(data.table)
    library(matrixStats)
    library(glue)
    
    # Myself packages 
    library(Ipaper)
    # library(phenology)
    library(phenofit)
    library(Rcmip5)

    library(sp)
    library(oce)
})

file_pheno_012    <- "OUTPUT/phenology_TP_AVHRR_phenofit.rda"
file_pheno_010    <- "OUTPUT/phenology_TP_AVHRR_phenofit_010deg.rda"

file_pheno_010       <- "OUTPUT/phenology_TP_AVHRR_phenofit_010deg.rda"
file_pheno_010_3s    <- "OUTPUT/phenology_TP_phenology_010deg_3s.rda"
file_pheno_010_3s_V2 <- "OUTPUT/phenology_TP_phenology_010deg_3s_V2.rda"

file_preseason <- "OUTPUT/TP_010deg_preseason2.rda"
file_plsr      <- "OUTPUT/TP_010deg_PLSR_SOS and Non-SOS_V2.rda"
file_plsr_mk   <- "OUTPUT/TP_010deg_PLSR_SOS and Non-SOS_(slope_mk).rda"

file_SPOT_010 <- "E:/SciData/pheno_SPOT_TP (1998-2013)/phenofit_SPOT_TP_010deg.RDS"

## 
# source('test/main_vis.R')
OS.type = .Platform$OS.type
if (OS.type == 'windows') {
    windowsFonts(
        Times = windowsFont("Times New Roman"), 
        Arial = windowsFont("Arial"), 
        YH = windowsFont("Microsoft Yahei"), 
        whit = windowsFont("Whitney-Book")
    )
} else if (OS.type == 'unix'){
    Cairo::CairoFonts(
        regular="Times New Roman:style=Regular",
        bold="Times New Roman:style=Bold",
        italic="Times New Roman:style=Oblique",
        bolditalic="Times New Roman:style=BoldOblique"
    )
}

theme_set( theme_bw(base_size = 14) + 
    theme(
        # legend.position = "none", 
        # panel.grid = element_blank(), 
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
