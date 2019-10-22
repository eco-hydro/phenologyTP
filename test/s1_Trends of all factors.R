## 还需要计算一个趋势
source("test/main_pkgs.R")
load("data/00basement_TP.rda")
load(file_pheno_010)
load(file_preseason)
ngrid <- length(gridclip2_10)

tidy_slope <- function(lst, I) {
    get_data <- function(k = 1) {
        df <- map(lst, ~.x[, k]) %>% do.call(rbind, .) %>% as.data.frame()
        fill_df_null(df, I)
    }
    
    l_slope  <- get_data(1)
    l_pvalue <- get_data(2)
    list(trend = l_slope, pvalue = l_pvalue)
}

# 2000 | 2001
years = 1982:2015
year0 = 2001
I_year0 <- which(years >= year0)[1]

cal_trend <- function(slope = slope_p, only.MODIS = FALSE){
    # test remove MODIS the first year
    sources <- c("GIMMS", "GIMMS before 2000", "GIMMS after 2001", "MCD12Q2", "SPOT")
    lst <- lst_preseason[c(1, 1, 1, 2, 3)] %>% set_names(sources)
    
    lst_trend <- foreach(l = lst, grp = icount()) %do% {
        runningId(grp, prefix = "grp")
        
        I_row = 1:nrow(l$data[[1]])
        if (grp == 2) {
            I_row = 1:(I_year0 - 1) # before
        } else if (grp == 3) {
            I_row = I_year0:34
        }
        
        if (only.MODIS) {
            # remove the first year
            if (grp != 4) return()
            I_row = I_row[-1]
        }
        
        ans <- foreach(d = l$data, i = icount()) %dopar% {
            runningId(i, 1000)
            tryCatch({
                foreach(x = d[I_row, ], .combine = rbind) %do% {
                    slope(x)
                }  %>% set_rownames(varnames)
            }, error = function(e){
                message(sprintf("[%d] %s", i, e$message))
                # matrix(NA, nrow(d), 2)
            })
        }
        I_del = sapply(ans, is.null) %>% which()
        I <- if (length(I_del) > 0) l$I[-I_del] else l$I
        
        res <- tidy_slope(ans, I)
    } 
    lst_trend
}

## PLSR result on Tibet Plateau, Dongdong Kong (20190403) ======================
# Update: kongdd, (20191003)
load(file_preseason)
d <- lst_preseason$GIMMS$data[[1]]
varnames <- colnames(d)

InitCluster(12)

l_lm       <- cal_trend(slope_p)
l_mk       <- cal_trend(slope_mk)

l_lm.MODIS <- cal_trend(slope_p, only.MODIS = TRUE)
l_mk.MODIS <- cal_trend(slope_mk, only.MODIS = TRUE)

# l_lm <- tidy_slope(lst_trend.lm)
# l_mk <- tidy_slope(lst_trend.mk)
save(l_lm, l_mk, l_lm.MODIS, l_mk.MODIS, file = file_trend)
