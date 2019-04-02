source("test/step0_tidy_phenofit_phenology.R")

## 2 pearson correlation -------------------------------------------------------
d_corr <- corr_matrix(df_SOS[, -1] %>% as.matrix() %>% t(), 
    df_EOS[, -1] %>% as.matrix() %>% t())

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




