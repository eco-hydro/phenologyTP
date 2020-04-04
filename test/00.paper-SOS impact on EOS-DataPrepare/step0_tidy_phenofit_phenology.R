## post-process
source('test/main_pkgs.R')
library(grid)
library(matrixStats)

# MAIN script ------------------------------------------------------------------
load("data/00basement_TP.rda")

indir <- "D:/Documents/OneDrive - mail2.sysu.edu.cn/phenology/AVHRR_TP_v0.1.2"
files <- dir(indir, pattern = "*.RDS", full.names = TRUE)
# file <- "D:/Documents/OneDrive - mail2.sysu.edu.cn/phenofit_20267.RDS"

lst_pheno <- llply(files, read_pheno, .progress = "text")
df_pheno <- lst_pheno %>% set_names(str_extract(files, "\\d{5}")) %>% melt_list("row")
df_pheno$row %<>% as.numeric()

df_pheno_avg <- df_pheno[, lapply(.SD, median, na.rm = T), .(row), 
                         .SDcols = colnames(df_pheno)[-c(1,ncol(df_pheno))]]

save(df_pheno, df_pheno_avg, file = file_pheno_012)
# source('F:/phenology/phenology/phenology_TP/test/Figure2_phenology_spatial_dist.R')

Fig1_data = TRUE
if (Fig1_data) {
    load(file_pheno_012)
    df_temp <- df_pheno[, .(row, year = year(origin), TRS2.sos, TRS6.eos)]
    # mask outlier
    df_temp[, `:=`(TRS2.sos = mask_outlier(TRS2.sos), TRS6.eos = mask_outlier(TRS6.eos)), .(row)]

    df_SOS <- df_temp %>% dcast(row~year, value.var = "TRS2.sos")
    df_EOS <- df_temp %>% dcast(row~year, value.var = "TRS6.eos")

    df_SOS_10deg <- resample2_10deg(gridclip, df_SOS, range) 
    df_EOS_10deg <- resample2_10deg(gridclip, df_EOS, range) 

    gridclip_10@data <- df_SOS_10deg
    gridclip_10@data <- df_EOS_10deg

    d_SOS_avg <- rowMeans2(df_SOS_10deg %>% as.matrix(), na.rm = TRUE)
    d_EOS_avg <- rowMeans2(df_EOS_10deg %>% as.matrix(), na.rm = TRUE)
    
    # rm all NA grids
    I_rem <- which(!(is.na(d_SOS_avg) | is.na(d_EOS_avg))) # about 2/3
    gridclip2_10 <- gridclip_10[I_rem, ]
    
    df_SOS_10deg %<>% .[I_rem, ] 
    df_EOS_10deg %<>% .[I_rem, ]
    d_SOS_avg    %<>% .[I_rem]
    d_EOS_avg    %<>% .[I_rem]
    I_grid2_10   <- I_grid_10[I_rem]
    
    save(df_SOS, df_EOS, df_SOS_10deg, df_EOS_10deg, d_EOS_avg, d_SOS_avg, 
        gridclip2_10, I_grid2_10, 
        file = file_pheno_010)
}
