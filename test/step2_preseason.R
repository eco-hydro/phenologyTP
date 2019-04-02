source("test/main_pkgs.R")
# source("../phenofit/test/load_pkgs.R")
# load("INPUT/phenology_TP_AVHRR_multi-annual.rda")
# colnames(df_pheno_avg)[1] <- "row"


s3_preseason = TRUE
if (s3_preseason) {
    source("test/main_pkgs.R")
    load("data/00basement_TP.rda") 
    load(file_pheno_010)
    # mete data are sampled according to I_grid_10
    I_rem <- match(I_grid2_10, I_grid_10)
    
    FUN_tidy <- .  %>% .[I_rem, -(1:365)] %>% t() # [time, grid]
    load("INPUT/TP_010deg_temp3.rda")
    lst_temp <- map(lst_temp, ~t(.x[, -(1:365)])) # rm 1981
    # lst_temp <- lst_temp[c(2, 3)] %>% map(FUN_tidy) # rm 1981
    gc()
    
    load("INPUT/TP_010deg_prec.rda")
    lst_prec %<>% do.call(cbind, .) %>% FUN_tidy
    
    load("INPUT/TP_010deg_srad.rda")
    lst_srad %<>% do.call(cbind, .) %>% FUN_tidy
    
    dates <- seq(ymd('19820101'), ymd('20151231'), by = "day")
    info_date <- data.table(date = dates, year = year(dates), yday = yday(dates))
    
    ngrid <- nrow(gridclip2_10)
    
    InitCluster(7)
    r <- foreach(
        Tmin = lst_temp$Tmin, 
        Tmax = lst_temp$Tmax,
        Prec = lst_prec, 
        Srad = lst_srad,
        d_sos = df_SOS_10deg %>% t(), 
        d_eos = df_EOS_10deg %>% t(), 
        i = icount(), 
        .packages = c("magrittr", "ppcor", "data.table", "foreach", "matrixStats", "phenology")
        # .export = c()
        ) %dopar% {
        phenofit::runningId(i, 100, ngrid)
        get_preseason(Tmin, Tmax, Prec, Srad, d_sos[, 1], d_eos[, 1], info_date)
    } 
    
    mat_preseason <- r %>% purrr::transpose() # 
    mat_preseason[1:2] %<>% map(~do.call(rbind, .))
    save(mat_preseason, file = file_preseason)
    
    ## 2. Autumn phenology model -----------------------------------------------
    load("data/00basement_TP.rda")
    load('OUTPUT/phenology_TP_AVHRR_phenofit_010deg.rda')
    
    file_preseason <- "OUTPUT/TP_010deg_preseason2.rda"
    load(file_preseason)
    
    ngrid <- length(gridclip2_10)
    
    # 2.1 max_pcor
    gridclip2_10@data <- as.data.frame(mat_preseason$pcor.max)
    brks <- critical_pcor(32, 3) %>% c(1) %>% c(-rev(.), 0, .) %>% round(3)

    g <- spplot_grid(gridclip2_10, colors = colors$corr, brks = brks, layout = c(2, 3), 
        pars = list(
            title = list(x=76.5, y=39, cex=1.5),
            hist = list(origin.x=77, origin.y=27, A=15, by = 0.5, axis.x.text = FALSE, ylab.offset = 2.5)))
    write_fig(g, "Figure_s2_max_pcor_v2.pdf", 10, 7.1)
    
    # 2.2 preseason pcor
    mat_pcor <- foreach(d = mat_preseason$data, i = icount(),.combine = "rbind") %do% {
            phenofit::runningId(i, 1000, ngrid)
            I_nona <- is.na(d) %>% rowSums2(na.rm=TRUE) %>% {which(. == 0)}
            d <- d[I_nona, ]
            res <- pcor(d)$estimate
            res <- res[-nrow(res), "EOS"]
        } %>% set_rownames(NULL)
    
    gridclip2_10@data <- as.data.frame(mat_pcor)
    brks <- critical_pcor(32, 3) %>% c(1) %>% c(-rev(.), 0, .) %>% round(3)
    
    g <- spplot_grid(gridclip2_10, colors = colors$corr, brks = brks, layout = c(2, 3), 
                     pars = list(
                         title = list(x=76.5, y=39, cex=1.5),
                         hist = list(origin.x=77, origin.y=27, A=15, by = 0.5, axis.x.text = FALSE, ylab.offset = 2.5)))
    write_fig(g, "Figure_preseason_pcor.pdf", 10, 7.1)
    
    # result indicates spring phenology has a weak influence on autumn phenology.
    d <- mat_pcor %>% data.table() %>% cbind(row = 1:nrow(.), .) %>% melt("row", variable.name="varname")

    ## 2.3 PLSR coefficient and contribution
    # InitCluster(6)
    # lst_pls <- foreach(d = mat_preseason$data, i = icount(),.combine = , 
    #                    .packages = c("magrittr", "phenology")) %dopar% {
    #         phenofit::runningId(i, 100, ngrid)
    #         I_nona <- is.na(d) %>% matrixStats::rowSums2(na.rm=TRUE) %>% {which(. == 0)}
    #         d <- d[I_nona, ] %>% as.matrix()
    #         X <- d[, 1:5] # METE + SOS
    #         Y <- d[, 6, drop=FALSE]   # EOS 
    #         
    #         plsreg1_adj(X, Y, comps = 2, autoVars = TRUE, nminVar = 2, minVIP = 0.8)
    #         # m_opls <- opls(X %>% as.matrix(), Y %>% as.matrix(), predI = 2, plotL = FALSE, printL=FALSE) # 32 times slower 
    #     } %>% 
    #     purrr::transpose() # %>% map(~do.call(rbind, .))
    # 
    # tidy_init <- . %>% map(~.[1, -1]) %>% do.call(rbind, .)
    # tidy_last <- . %>% map(~.[2, -1]) %>% do.call(rbind, .)
    # 
    # pls_init <- map(lst_pls, tidy_init)
    # pls_last <- map(lst_pls, tidy_last)
    # 
    # save(pls_init, pls_last, file = "OUTPUT/TP_010deg_pls_preseason_OUTPUT.rda")

    ## 2.4 The difference of considering SOS or not
    InitCluster(6)
    lst_pls <- foreach(d = mat_preseason$data, 
                       i = icount(),.combine = , 
                       .packages = c("magrittr", "phenology")) %dopar% {
            phenofit::runningId(i, 100, ngrid)
            plsr_attributable(d)
            # need predictions
        } %>% 
        purrr::transpose() %>% map(function(l){
            ans <- transpose(l) 
            ans %>% map(~do.call(rbind, .))
        })
    
    save(lst_pls, file = "OUTPUT/TP_010deg_PLSR_SOS and nonSOS.rda")
    
    # 可能还需要置信区间
    load("OUTPUT/TP_010deg_PLSR_SOS and nonSOS.rda")
    
    {
        Cairo::CairoPDF("FIgure5_SOS and non-SOS model.pdf", 7, 3)
        par(mar = c(3, 3, 1, 1), mgp = c(1.8, 0.6, 0))
        
        lwd <- 1.5
        Year <- 1982:2015
        df_EOS_10deg %>% as.matrix() %>% colMeans2(na.rm = T) %>% 
            plot(Year, ., col = "black", type = "b", pch = 21, bg = "grey80", lwd = lwd, 
                 ylab = "EOS (day of year)", font.lab = 2, cex.axis = 1, cex.lab=1); grid()
        lst_pls$SOS$ypred %>% colMeans2(na.rm = T) %>% lines(Year, ., col = "blue", lwd = lwd); grid()
        lst_pls$nonSOS$ypred %>% colMeans2(na.rm = T) %>% lines(Year, ., col = "red", lwd = lwd); grid()
        legend("topleft", c("Observation", "SOS model", "non-SOS model"), lty = 1, 
               col = c("black", "blue", "red"), pch = c(1, NA, NA))
        dev.off()
    }
    
    ## over all prmse
    d <- map(lst_pls, ~.$Q2[, "PRESS"]) %>% as.data.frame() %>% cbind(row = 1:ngrid, .) %>% 
        melt("row") %>% data.table()
    ggplot(d, aes(variable, value, fill = variable)) + 
        stat_boxplot(geom = "errorbar", width = 0.5)  + 
        geom_boxplot2()
    
    tidy_init <- . %>% map(~.[1, -1]) %>% do.call(rbind, .)
    tidy_last <- . %>% map(~.[2, -1]) %>% do.call(rbind, .)
    
    pls_init <- map(lst_pls, tidy_init)
    pls_last <- map(lst_pls, tidy_last)
}


g_diff <-
    ggplot(d, aes(variable, value, fill = variable)) +
    stat_boxplot(geom = "errorbar", width = 0.5) + 
    geom_boxplot2() + 
    stat_summary(fun.data = label_sd, colour = "black", size = 4, geom = "text", vjust = -0.5) + 
    theme(legend.position = "none") + 
    labs(x = NULL, y = "PRESS")
write_fig(g_diff, "Figure5a_diff of sos and non-sos.pdf", 4, 4)
# 3.1 CHECK PLSR stepwise RESULT
{
    # load("OUTPUT/TP_010deg_pls_preseason_OUTPUT.rda")
    basesize <- 15
    g1 <- pls_show(pls_init, basesize)
    g2 <- pls_show(pls_last, basesize)
    
    write_fig(g2, "Figure4_PLSR_last.pdf", 12, 7)
    write_fig(g1, "Figure4_PLSR_init.pdf", 12, 7)
    write_fig(g1, "Figure4_PLSR_init.tif", 12, 7)

    d <- data.table(row = 1:ngrid, init = pls_init$Q2$PRESS, last = pls_last$Q2$PRESS) %>% 
        melt("row")
    ggplot(d, aes(variable, value, fill = variable)) + 
        stat_boxplot(geom ='errorbar', width = 0.5) +
        geom_boxplot2() + 
        scale_x_discrete(labels = c("PLSR with all variables", "Stepwise PLSR")) +
        labs(x = NULL, y = "PRESS") + 
        theme(legend.position = "none")
    # ggplot(d, aes(init, last)) + 
    #     geom_hex() + 
    #     geom_density2d() + 
    #     geom_abline(color = "red", size = 1) + 
    #     coord_equal() + 
    #     scale_color_manual(values = RColorBrewer::brewer.pal(9, "Blues"))
    # plot(,); abline(a = 0, b = 1, col = "red")
    # hist(pls_init$Q2$PRESS - pls_last$Q2$PRESS)
}

##下一步测试包含和未包含SOS时模型的预测差别： PRMSE and Trend
# 圈出SOS显著地区域
