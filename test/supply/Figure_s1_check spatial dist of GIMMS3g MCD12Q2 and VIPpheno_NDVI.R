source("test/main_pkgs.R")

# Get spatial mean of 3 dataset.

read_MCD12Q2 <- function(file){
    rgdal::readGDAL(file, silent = TRUE)[seq(1, 8, 2)]@data 
}
read_VIPpheno <- function(file){
    rgdal::readGDAL(file, silent = TRUE)@data
}

pheno_MOD      <- c("Greenup", "Maturity", "Senescence", "Dormancy")
pheno_VIP      <- c("VIPpheno_SOS", "VIPpheno_EOS")
pheno_GIMMS    <- c("GIMMS_SOS", "GIMMS_EOS")

load("data/00basement_TP.rda")
load(file_pheno_010)

## MAIN ------------------------------------------------------------------------
root <- "D:/Documents/OneDrive - mail2.sysu.edu.cn/SciData/TP_phenology_010deg"
dirs <- dir(root, full.names = TRUE) # mcd12q2 and VIPpheno_NDVI

lst_MCD12Q2 <- dir(dirs[1], "*.tif", full.names = TRUE) %>% 
    llply(read_MCD12Q2) %>% purrr::transpose() %>% 
    map(~do.call(cbind, .)) %>% 
    set_names(pheno_MOD)
df_avg <- map(lst_MCD12Q2, ~rowMeans2(., na.rm = TRUE)) %>% as.data.frame()

lst_VIPpheno <- dir(dirs[2], "*.tif", full.names = TRUE) %>% 
    llply(read_VIPpheno) %>% purrr::transpose() %>% 
    map(~do.call(cbind, .)) %>% 
    set_names(pheno_VIP)
df_avg_VIPpheno <- map(lst_VIPpheno, ~rowMeans2(., na.rm = TRUE)) %>% as.data.frame()
df_avg %<>% cbind(df_avg_VIPpheno)
df_avg <- cbind(df_avg[I_grid2_10, ], GIMMS_SOS = d_SOS_avg, GIMMS_EOS = d_EOS_avg)
rm(df_avg_VIPpheno)
# gridclip2_10@data <- df_avg#[I_grid2_10, ]
# spplot(gridclip2_10)

## 2. prepare inter-annual phenology metrics Time-series
lst_pheno <- list(
    GIMMS = list(SOS = df_SOS_10deg, EOS = df_EOS_10deg) %>% map(as.matrix),
    MCD12Q2 = lst_MCD12Q2[c(1, 4)] %>% map(~.[I_grid2_10, ] %>% set_colnames(2001:2014)),
    VIP_pheno = map(lst_VIPpheno, ~.[I_grid2_10, -1] %>% set_colnames(1982:2014))
) %>% map(~set_names(., c("SOS", "EOS")))

save(lst_pheno, gridclip2_10, I_grid2_10, file = file_pheno_010_3s)

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