# source("test/main_pkgs.R")
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

library(Rcmip5)
library(Ipaper)
library(data.table)
library(matrixStats)


file_pheno_012    <- "OUTPUT/phenology_TP_AVHRR_phenofit.rda"
file_pheno_010    <- "OUTPUT/phenology_TP_AVHRR_phenofit_010deg.rda"

file_pheno_010    <- "OUTPUT/phenology_TP_AVHRR_phenofit_010deg.rda"
file_pheno_010_3s <- "OUTPUT/phenology_TP_phenology_010deg_3s.rda"

file_preseason <- "OUTPUT/TP_010deg_preseason2.rda"

## 
theme_set( theme_bw(base_size = 14) + 
    theme(panel.grid.minor = element_blank()))

A     = 20
ntick = 2
by    = 0.6
