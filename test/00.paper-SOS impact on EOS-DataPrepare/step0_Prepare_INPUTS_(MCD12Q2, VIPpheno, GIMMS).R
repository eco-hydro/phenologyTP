source("test/main_pkgs.R")

root <- "D:/Documents/OneDrive - mail2.sysu.edu.cn/SciData/TP_phenology_010deg"
load("data/00basement_TP.rda")
load(file_pheno_010)

## GLOBAL FUNCTIONS ------------------------------------------------------------
# Get spatial mean of 3 dataset.
read_MCD12Q2_V6 <- function(file){
    I_V6    <- c(1, 2, 7, 8, 9, 10, 13, 14)+1
    I_bands <- I_V6[seq(1, 8, 2)]
    x = rgdal::readGDAL(file, silent = TRUE)[I_bands]
    
    year <- str_extract(basename(file), "\\d{4}") %>% as.numeric()
    
    
    doy_first <- { make_date(year, 1, 1) - 1 } %>% as.numeric()
    x@data[x@data == 0] = NA
    x@data %<>% subtract(doy_first)
    
    x@data
}

read_MCD12Q2 <- function(file){
    rgdal::readGDAL(file, silent = TRUE)[seq(1, 8, 2)]@data
    # + 1
}

read_VIPpheno <- function(file){
    rgdal::readGDAL(file, silent = TRUE)@data
}

pheno_MOD      <- c("Greenup", "Maturity", "Senescence", "Dormancy")
pheno_VIP      <- c("VIPpheno_SOS", "VIPpheno_EOS")
pheno_GIMMS    <- c("GIMMS_SOS", "GIMMS_EOS")

types = c("MCD12Q2_V5", "MCD12Q2_V6", "VIPpheno_NDVI") %>% 
    set_names(., .)

lst_pheno <- foreach(type = types, i = icount()) %do% {
    runningId(i)
    FUN = switch(type, 
        MCD12Q2_V5 = read_MCD12Q2, 
        MCD12Q2_V6 = read_MCD12Q2_V6, 
        VIPpheno_NDVI = read_VIPpheno)
    indir <- file.path(root, type)
    files <- dir(indir, "*.tif", full.names = TRUE)
    
    l <- llply(files, FUN) %>% purrr::transpose() %>% 
        map(~do.call(cbind, .))
    if (length(l) == 4) {
        l <- l[c(1, 4)] # Greenup, Dormancy
        # names = pheno_MOD
    } #else 
    names = c("SOS", "EOS")
    set_names(l, names) %>% map(~.[I_grid2_10, ])
}

lst_pheno$GIMMS = list(SOS = df_SOS_10deg, EOS = df_EOS_10deg) %>% map(as.matrix)
save(lst_pheno, gridclip2_10, I_grid2_10, file = file_pheno_010_3s_V2)

df <- map_depth(lst_pheno, 2, ~rowMeans2(., na.rm = TRUE)) %>% 
    map(~as.data.table(.) %>% cbind(I_grid = I_grid2_10, .)) %>% melt_list("type_source")
# dcast(df, I_grid~type_source, value.var = "EOS")

# write_fig(spplot(x5, as.table = TRUE), "v5.tif", 8, 10)
# write_fig(spplot(x6[1:8], as.table = TRUE), "v6.tif", 8, 10)

## MAIN ------------------------------------------------------------------------
# resampled to 0.1 deg first
# df_avg <- map(lst_MCD12Q2, ~rowMeans2(., na.rm = TRUE)) %>% as.data.frame()
# df_avg_VIPpheno <- map(lst_VIPpheno, ~rowMeans2(., na.rm = TRUE)) %>% as.data.frame()
# df_avg %<>% cbind(df_avg_VIPpheno)
# df_avg <- cbind(df_avg[I_grid2_10, ], GIMMS_SOS = d_SOS_avg, GIMMS_EOS = d_EOS_avg)
# rm(df_avg_VIPpheno)
# gridclip2_10@data <- df_avg#[I_grid2_10, ]
# spplot(gridclip2_10)

## 2. prepare inter-annual phenology metrics Time-series

## Figure 1. simple correlation analysis
# source('test/supply/Figure_s1_check spatial dist of GIMMS3g MCD12Q2 and VIPpheno_NDVI.R')    


## 1. spatial dist ------------------------------------------------------------------
Fig_s1 = FALSE
if (Fig_s1){       
    # ps <- foreach(d_avg = lst_avg, meth = names(lst_avg)) %do% {
    # gridclip <- fill_grid(gridclip, d_avg)
    # gridclip <- fill_grid(gridclip, df_pheno_avg)
    # gridclip@data <- df_pheno_avg[, -1] %>%  as.data.frame()    
    pars <- list(
        title = list(x=77, y=39, cex=1.5),
        hist = list(axis.x.text = FALSE, ylab.offset = 2.5, origin.x = 77)
    )
    toFactor <- FALSE
    varnames <- c(pheno_GIMMS, pheno_MOD[c(1, 4)], pheno_VIP)
    titles_a <- c(expression(bold("(a) "*GIMMS[3*g]*" SOS")), 
                expression(bold("(b) "*GIMMS[3*g]*" EOS")))
    titles_b <- c("MCD12Q2", "VIP_Pheno") %>% rep(each = 2) %>% paste(c("SOS", "EOS"), sep = " ") %>% 
                    sprintf("(%s) %s", letters[3:6], .)

    ps = foreach(varname = varnames, i = icount()) %do% {
        title = ifelse (i <= 2, titles_a[i], titles_b[i-2])
        brk <- if (i %% 2 == 0) brks$EOS else brks$SOS
        color <- if (i %% 2 == 0) colors$EOS else colors$SOS
        spplot_grid(gridclip2_10, varname, #[-c(7, 9)], 
             panel.title = title, #sprintf("(%s) %s", letters[i], titles[i] 
             brks = brk, colors = color, pars = pars, toFactor = toFactor)
    }
    
    g <- arrangeGrob(grobs = ps, nrow = 3)
    write_fig(g, "Figure_s1_3sources_multi-annual phenology dist.pdf", 10.5, 7.5)
    write_fig(g, "Figure_s1_3sources_multi-annual phenology dist.tif", 10.5, 7.5)
}

# "Onset_Greenness_Increase1", 
# "Onset_Greenness_Increase2", 
# "Onset_Greenness_Maximum1", 
# "Onset_Greenness_Maximum2", 
# "Onset_Greenness_Decrease1", 
# "Onset_Greenness_Decrease2", 
# "Onset_Greenness_Minimum1", 
# "Onset_Greenness_Minimum2",
