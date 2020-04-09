# setwd("phenology_TP")
source("test/main_pkgs.R")

root <- path.mnt("D:/Documents/OneDrive - mail2.sysu.edu.cn/SciData/TP_phenology_010deg")
load("data/00basement_TP.rda")

i_grid_010_cliped <- raster::extract(raster(grid_010.TP), grid_010.TP_cliped)
# grid_010.TP_cliped@data <- data.table(id = ind)
# use_data(grid_010.TP_cliped, overwrite = TRUE)

## GLOBAL FUNCTIONS ------------------------------------------------------------
pheno_MOD <- c("Greenup", "Maturity", "Senescence", "Dormancy")
pheno_VIP <- c("VIPpheno_SOS", "VIPpheno_EOS")
pheno_GIMMS <- c("GIMMS_SOS", "GIMMS_EOS")

types <- c("MCD12Q2_V5", "MCD12Q2_V6", "VIPpheno_EVI2", "VIPpheno_NDVI") %>%
    set_names(., .)

lst_pheno <- foreach(type = types, i = icount()) %do% {
    runningId(i)
    FUN <- switch(type,
        MCD12Q2_V5 = read_MCD12Q2,
        MCD12Q2_V6 = read_MCD12Q2_V6,
        VIPpheno_EVI2 = read_VIPpheno,
        VIPpheno_NDVI = read_VIPpheno
    )
    indir <- file.path(root, type)
    files <- dir(indir, "*.tif", full.names = TRUE)

    l <- llply(files, FUN) %>%
        purrr::transpose() %>%
        map(~ do.call(cbind, .))
    # if (length(l) == 4) {
    #     l <- l[c(1, 4)] # Greenup, Dormancy
    #     # names = pheno_MOD
    # } # else
    # names <- c("SOS", "EOS")
    map(l, ~ .[i_grid_010_cliped, ])
}

# V5: 2001-2014
# V6: 2001-2017
# VIPpheno: 1981-2014
lst_pheno$MOD13C1 <- readRDS(file_MOD13C1_010)
lst_pheno$SPOT <- readRDS(file_SPOT_010)
lst_pheno$GIMMS3g <- readRDS(file_AVHRR_010)

saveRDS(lst_pheno, file_pheno_full)
# lst_pheno <- readRDS(file_pheno_full)

## 2. select phenological metrics -------------------------------------------------
sources <- c("MCD12Q2_V5", "MCD12Q2_V6", "VIPpheno_EVI2", "VIPpheno_NDVI", "MOD13C1", "SPOT", "GIMMS3g")
lst_time <- list(
    2001:2014,
    2001:2017, 
    1981:2014, 
    1981:2014,
    2000:2018,
    1998:2013, 
    1982:2015
) %>% set_names(sources)

# select metrics first
# metrics = c("Greenup" , "DER.pop", "TRS6.eos") # Dormancy
l_pheno <- foreach(l = lst_pheno, years = lst_time, i = icount()) %do% {
    if (i %in% c(1, 2)) {
        names = c("Greenup", "Peak", "Dormancy")
        i_metric <- match(names, names(l))
        ans <- l[i_metric] %>% set_names(c("SOS", "POP", "EOS"))
    } else if (i >= 3 && i <= 4) {
        ans <- l
    } else {
        i_metric <- match(c("TRS2.sos", "DER.pop", "TRS6.eos"), metrics_all)
        ans <- map(l, ~ as.data.table(.[, i_metric])) %>%
            purrr::transpose() %>%
            map(~ do.call(cbind, .) %>% set_dirnames(NULL)) %>% 
            set_names(c("SOS", "POP", "EOS"))
    } 
    ans$year <- years
    ans
} 
saveRDS(l_pheno, file_pheno)
