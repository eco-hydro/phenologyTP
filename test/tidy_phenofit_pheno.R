## post-process
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

save(df_pheno, df_pheno_avg, file = "OUTPUT/phenology_TP_AVHRR_phenofit.rda")
# source('F:/phenology/phenology/phenology_TP/test/Figure2_phenology_spatial_dist.R')

Fig1_data = TRUE
if (Fig1_data) {
    df_temp <- df_pheno[, .(row, year = year(origin), TRS2.sos, TRS6.eos)]
    # mask outlier
    df_temp[, `:=`(TRS2.sos = mask_outlier(TRS2.sos), TRS6.eos = mask_outlier(TRS6.eos)), .(row)]

    df_SOS <- df_temp %>% dcast(row~year, value.var = "TRS2.sos")
    df_EOS <- df_temp %>% dcast(row~year, value.var = "TRS6.eos")

    df_SOS_10deg <- resample2_10deg(gridclip, df_SOS) 
    df_EOS_10deg <- resample2_10deg(gridclip, df_EOS) 

    gridclip_10@data <- df_SOS_10deg
    gridclip_10@data <- df_EOS_10deg

    d_SOS_avg <- rowMeans2(df_SOS_10deg %>% as.matrix(), na.rm = TRUE)
    d_EOS_avg <- rowMeans2(df_EOS_10deg %>% as.matrix(), na.rm = TRUE)

    save(df_SOS, df_EOS, df_SOS_10deg, df_EOS_10deg, d_EOS_avg, d_SOS_avg, 
        file = "OUTPUT/phenology_TP_AVHRR_phenofit_010deg.rda")    
}

## 2 pearson correlation -------------------------------------------------------
d_corr <- foreach(sos = iter(df_SOS[, -1] %>% as.matrix() %>% t()), 
    eos = iter(df_EOS[, -1] %>% as.matrix() %>% t()), 
    i = icount(),
    .combine = "rbind") %do% {
    tryCatch({
        sos2 <- pracma::detrend(sos)
        eos2 <- pracma::detrend(eos)
        r <- cor.test(sos2, eos2, use = "pairwise.complete.obs")
        c(R = r$estimate[[1]], pvalue = r$p.value)
    }, error = function(e){
        message(sprintf("[e] %d: %s", i, e$message))
        c(R = NA_real_, pvalue = NA_real_)
    })
} %>% data.table()

d_corr %<>% cbind(row = df_EOS$row, .)
d_corr[, level := cut(pvalue, c(0, 0.05, 0.1, 0.2, 1))]

ggplot(d_corr, aes(R, fill = level)) + geom_histogram() + 
    geom_vline(xintercept = 0, color = "black") + 
    geom_vline(xintercept = mean(d_corr$R), color = "red", linetype = 2)
    
gridclip2 <- fill_grid(gridclip, d_corr %>% data.frame())

## 3. MKtrend (or EOF analysis?) -----------------------------------------------
Fig3_data = FALSE
if (Fig3_data) {
    lst_trend <- foreach(sos = iter(df_SOS[, -1] %>% as.matrix() %>% t()), 
    eos = iter(df_EOS[, -1] %>% as.matrix() %>% t()), 
    i = icount()
    # .combine = "combine"
    ) %do% {
        phenofit::runningId(i, 1000)
        
        tryCatch({
            t_sos <- mkTrend(sos)
            t_eos <- mkTrend(eos)
            list(SOS = t_sos, EOS = t_eos)
        }, error = function(e){
            message(sprintf("[e] %d: %s", i, e$message))
        })
    }
    d_trend <- transpose(lst_trend) %>% 
        map(~do.call(rbind, .) %>% data.table %>% cbind(row = df_EOS$row, .))

    save(d_trend, file = "pheno_Trend.rda")
}

Fig3_trend = TRUE
if (Fig3_trend) {
    load("pheno_Trend.rda")
    ps_trend <- foreach(d = d_trend, name=names(d_trend), i = icount()) %do% {
        d <- data.frame(d) %>% data.table() # avoid modify d_trend

        d[, level := cut_pvalue(pval, slp)]
        d[, slp := slp*10]
        gridclip2 <- fill_grid(gridclip, d)
        
        p_pval <- spplot_grid(gridclip2, "level", 
                              panel.title = "",
                              colors = colors$pvalue, sub.hist = FALSE, colorkey = FALSE)
        g_pval <-  grid.grabExpr( plot(p_pval, more = T) )
            
        grid.newpage()
        title <- sprintf("(%s) %s (days/10a)", letters[i], name)
        spplot_grid(gridclip2, "slp", 
                             panel.title = title, 
                             pars = list(title = list(x=93, y=42, cex=1.5), 
                                         hist = list(origin.x=76, origin.y=27, A=20, by = 0.6)), 
                             brks = c(0.5, 1, 3, 5, Inf) %>% c(-rev(.), 0, .), 
                             colors = colors$SOS,
                             grob = g_pval, bbox = c(0, 0.5, 0.5, 1.15),
                             xlim = xlim + c(0, -0.2),
                             ylim = ylim + c(-0.1, 4), legend.space = "bottom")
    }
    g <- arrangeGrob(grobs = ps_trend, nrow = 1)
    write_fig(g, "Figure3_phenology_trend.pdf", 12, 4.6)
}

## 4. Time series of phenology (spatial average) -------------------------------

Fig4_multiYear_trend = TRUE
if (Fig4_multiYear_trend) {
    load('OUTPUT/phenology_TP_AVHRR_phenofit_010deg.rda')

    y_SOS <- df_SOS[, -1] %>% as.matrix() %>% colMeans2(na.rm = T)
    y_EOS <- df_EOS[, -1] %>% as.matrix() %>% colMeans2(na.rm = T)

    sd_SOS <- df_SOS[, -1] %>% as.matrix() %>% colSds(na.rm = T)
    sd_EOS <- df_EOS[, -1] %>% as.matrix() %>% colSds(na.rm = T)

    d <- list(SOS = data.frame(year = 1982:2015, y = y_SOS, sd = sd_SOS), 
              EOS = data.frame(year = 1982:2015, y = y_EOS, sd = sd_EOS)) %>% 
        map(~mutate(., ymin = y - sd, ymax = y+sd))
        # melt_list("phase")

    info_trend <- list(
        SOS = piecewise(y_SOS),
        EOS = piecewise(y_EOS) 
    ) %>% purrr::transpose()
    # we argue that SOS has a week affect on autumn phenology.
    
    offset <- 120
    lwd <- 0.95
    ggplot(d$SOS, aes(year, y)) + 
        geom_line(size = lwd) + 
        geom_line(data = d$EOS, aes(year, y - offset), color = "red", size =lwd) + 
        # geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.5, fill = "grey60") + 
        # facet_wrap(~phase, scales = "free_y", ncol = 1) + 
        scale_y_continuous("SOS", sec.axis = sec_axis(~.+offset, name = "EOS")) + 
        theme(axis.text.y.right = element_text(color = "red"), 
              axis.ticks.y.right = element_line(color = "red"), 
              axis.title.y.right = element_text(color = "red"), 
              axis.text = element_text(size = 13), 
              axis.title = element_text(size = 14))
}

## 5. Autumn phenology model ---------------------------------------------------

#' tryRcmip5_resample = FALSE
#' if (tryRcmip5_resample) {
#'     cellsize_12 <- 1/12
#'     lat_12 <- seq(range[1]+cellsize_12/2, range[2], cellsize_12)
#'     lon_12 <- seq(range[3]+cellsize_12/2, range[4], cellsize_12)
#'         
#'     #' nlat <- length(lat); nlon <- length(lon)
#'     #' ntime <- 10
#'     I <- match(1:nrow(grid), I_grid)
#'     
#'     # r <- interp3d_bilinear(list(lon = lon_12, lat = lat_12), 
#'     #                        data[I, ], range = range, cellsize_x = 1/10)
#'     # 
#'     grid_12 <- get_grid(range, cellsize_12)
#'     # 
#'     # mat_10 <- r$value 
#'     # mat_10 <- array(mat_10, dim = c(length(lat_10)*length(lon_10), dim(mat_10) %>% last()))
#'     # 
#'     # grid_10@data <- as.data.frame(mat_10)
#'     grid_10@data <- do.call(cbind, r) %>% data.frame()
#'     spplot(grid_10, 1:4)
#' }




