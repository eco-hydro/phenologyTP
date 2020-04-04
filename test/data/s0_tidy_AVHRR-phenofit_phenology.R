## post-process
# setwd("phenology_TP")
source('test/main_pkgs.R')
load_all()

# MAIN script ------------------------------------------------------------------
load("data/00basement_TP.rda")

if (!file.exists(file_pheno_012)) {
    indir <- "D:/Documents/OneDrive - mail2.sysu.edu.cn/phenology/AVHRR_TP_v0.1.2"
    files <- dir(indir, pattern = "*.RDS", full.names = TRUE)
    # file <- "D:/Documents/OneDrive - mail2.sysu.edu.cn/phenofit_20267.RDS"
    
    lst_pheno <- llply(files, read_pheno, .progress = "text")
    df_pheno <- lst_pheno %>%
        set_names(str_extract(files, "\\d{5}")) %>%
        melt_list("row")
    df_pheno$row %<>% as.numeric()
    
    df_pheno_avg <- df_pheno[, lapply(.SD, median, na.rm = T), .(row),
        .SDcols = colnames(df_pheno)[-c(1, ncol(df_pheno))]
    ]
    save(df_pheno, df_pheno_avg, file = file_pheno_012)
}

## fill_grid -------------------------------------------------------------------
load(file_pheno_012)
lst <- split(df_pheno, df_pheno$flag)
# lst_id <- map(lst, ~.$row)
# do.call(c, lst_id) %>% unique() %>% length()
grid <- grid_012.TP_cliped
ngrid <- nrow(grid)

r_target <- raster(grid_010.TP) # cliped is not work for raster
I <- raster::extract(r_target, grid_010.TP_cliped)

d <- lst[[1]]
lst_010 <- map(lst, function(d){
    grid <- fill_grid(grid_012.TP_cliped, d[, c(20, 3:19)]) 
    r2 <- raster::resample(brick(grid), r_target) # in 0.1 deg
    mat <- raster::extract(r2, grid_010.TP_cliped)  # calculate overlap
    mat
})

saveRDS(lst_010, file = file_pheno_010)
grid <- grid_010.TP_cliped
grid@data <- as.data.frame(x)

write_fig({
    print(spplot(grid))
    # plot(r2)
    # spplot(grid_010.TP_cliped)
}, "b.png", 12, 6, show = FALSE)

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
