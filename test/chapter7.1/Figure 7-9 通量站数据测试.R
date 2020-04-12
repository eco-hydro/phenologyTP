source("test/main_pkgs.R")

df <- fread("E:/Research/phenology/rfluxnet/OUTPUT/fluxsites166_FULLSET_daily_v20200411 (80%).csv")

vars <- colnames(df) %>% .[grep("QC_", .)] %>% gsub("QC_", "", .)
d_mean <- df[, lapply(.SD, mean, na.rm = TRUE), .(site, year), .SDcols = vars]
d_days <- df[, lapply(.SD, . %>% {sum(is.na(.))}), .(site, year), .SDcols = vars]
