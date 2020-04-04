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

cal_trend <- function(l, I_row, slope) {
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
    res
}

cal_trend_main <- function(slope = slope_p, only.MODIS = FALSE){
    years = 1982:2015
    year0 = 2000
    I_year0 <- which(years >= year0)[1]

    # test remove MODIS the first year
    sources <- c("GIMMS", "GIMMS before 2000", "GIMMS after 2001", "MCD12Q2", "SPOT", "MOD13C1")
    lst <- lst_preseason[c(1, 1, 1, 2, 3, 4)] %>% set_names(sources)

    lst_trend <- foreach(l = lst, grp = icount()) %do% {
        runningId(grp, prefix = "grp")
        # if (grp < 6) return()
        I_row = 1:nrow(l$data[[1]])
        if (grp == 2) {
            I_row = 1:(I_year0 - 1) # before
        } else if (grp == 3) {
            I_row = I_year0:34
        }
        
        if (only.MODIS) {
            if (grp != 4) return()
            I_row = I_row[-1] # remove the first year
        }
        cal_trend(l, I_row, slope)
    } 
    lst_trend
}

cal_trend_main2 <- function(slope = slope_p){
    I_rows <- list(
        19:34, # GIMMS3g 2000-2015
        1 :16, # MODIS   2000-2015
        3 :16  # SPOT    2000-2013
    )
    # sources <- c("GIMMS", "MOD13C1","SPOT")
    lst <- lst_preseason[c(1, 4, 3)]
    lst_trend <- foreach(l = lst, I_row = I_rows, grp = icount()) %do% {
        runningId(grp, prefix = "grp")
        # if (grp < 6) return()
        cal_trend(l, I_row, slope)
    } 
    lst_trend
}

## PLSR result on Tibet Plateau, Dongdong Kong (20190403) ======================
# Update: kongdd, (20191003)
load(file_preseason)
d <- lst_preseason$GIMMS$data[[1]]
varnames <- colnames(d)

InitCluster(12)

l_lm    <- cal_trend_main(slope_p)
l_mk    <- cal_trend_main(slope_mk)

# MODIS  : remove the first year 2001
l_lm.MODIS   <- cal_trend_main(slope_p, only.MODIS = TRUE)
l_mk.MODIS   <- cal_trend_main(slope_mk, only.MODIS = TRUE)

## second: GIMMS 2000-2015, MODIS 2000-2015, SPOT: 2000-2013 ===================
l_lm.main    <- cal_trend_main2(slope_p)
l_mk.main    <- cal_trend_main2(slope_mk)

save(l_lm, l_mk, l_lm.MODIS, l_mk.MODIS, 
     l_lm.main, l_mk.main,
     file = file_trend)

# l_lm.final = l_lm$GIMMS,
