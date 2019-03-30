source("test/main_pkgs.R")
# source("../phenofit/test/load_pkgs.R")
# load("INPUT/phenology_TP_AVHRR_multi-annual.rda")
# colnames(df_pheno_avg)[1] <- "row"

## 1. 计算积温生长季长度 --------------------------------------------------------
system.time({
    killCluster()
    InitCluster(8)

    indir <- "G:/SciData/中国数据/Data_forcing_01dy_010deg"
    lst_pheno <- foreach(year = 1981:2015, j = icount()) %do% {
        # 1. read Tavg
        file <- sprintf("%s/temp_ITPCAS-CMFD_V0106_B-01_01dy_010deg_%d01-%d12.nc", indir, year, year)
        r <- ncread_cmip5(file, "temp", range = range, ntime = -1, 
                      shiftLon = FALSE, delta = 0, offset = -273.15)
        # 2. Thermal growing season
        mat <- r$value %>% array_3dTo2d(I_grid)

        pheno <- foreach(x = iter(mat, "row"), i = icount()) %dopar% {
            phenofit::runningId(i, 1000, prefix = year)
            phenology::PhenoThermal(x, T_SOS = 0, T_EOS = 0, nday_SOS = 5, nday_EOS = 5, IsPlot = FALSE)
            # title(main = i)
        }
        do.call(rbind, pheno) %>% as.data.table()
    } 
    df_pheno <- lst_pheno %>% map(~cbind(row = 1:nrow(.), .)) %>% set_names(1981:2015) %>% melt_list("year")
    df_pheno_avg <- df_pheno[, lapply(.SD, mean, na.rm = T), .(row), .SDcols = colnames(df_pheno)[2:4]]
    save(lst_pheno, df_pheno, df_pheno_avg, file = "PhenoThermal_0_0.rda")
    # save(lst_pheno, file = )
})


## 2. 3hourly temperature to Tmin, Tmax, Tavg ----------------------------------
temp3h_ToDaily = FALSE
if (temp3h_ToDaily) {
    files <- dir("F:/ChinaForcing/Temp3h/", "*.nc$", full.names = T)
    file  <- files[1]
    
    lst_temp <- foreach(file = files, i = icount()) %do% {
        runningId(i, 1, length(files))
        r <- ncread_cmip5(file, "temp", range = range, ntime = -1, 
                          shiftLon = FALSE, delta = 0, offset = -273.15)
        
        mat_Tavg <- array_3dTo2d(r$value, I_grid_10) %>% array(dim = c(nrow(.), 8, ncol(.)/8)) %>% apply_3d(dim = 2)
        mat_Tmax <- array_3dTo2d(r$value, I_grid_10) %>% array(dim = c(nrow(.), 8, ncol(.)/8)) %>% apply_3d(dim = 2, FUN = rowMaxs)
        mat_Tmin <- array_3dTo2d(r$value, I_grid_10) %>% array(dim = c(nrow(.), 8, ncol(.)/8)) %>% apply_3d(dim = 2, FUN = rowMins)
        
        list(Tavg = mat_Tavg, Tmax = mat_Tmax, Tmin = mat_Tmin)
    } 
    # lst_temp <- lst_temp %>% purrr::transpose()
    lst_temp <- lst_temp %>% purrr::transpose() %>% map(~do.call(cbind, .))
    save(lst_temp, file = "TP_010deg_temp3.rda")
    # fwrite(lst_temp$Tavg, "TP_010deg_Tavg.csv")
}

# split into years
# lst_temp2 <- purrr::transpose(lst_temp) %>% map(~do.call(cbind, .))
# d <- data.table(Tavg = mat_Tavg[, i], Tmax = mat_Tmax[, i], Tmin = mat_Tmin[, i])

s3_preseason = TRUE
if (s3_preseason) {
    load("data/00basement_TP.rda")
    load('OUTPUT/phenology_TP_AVHRR_phenofit_010deg.rda')
    I_rem <- which(!(is.na(d_SOS_avg) | is.na(d_EOS_avg))) # about 2/3
    gridclip2_10 <- gridclip_10[I_rem, ]
        
    FUN_tidy <- .  %>% .[I_rem, -(1:365)] %>% t() # [time, grid]
    load("INPUT/TP_010deg_temp3.rda")
    lst_temp <- lst_temp[c(2, 3)] %>% map(FUN_tidy) # rm 1981
    gc()
    
    load("INPUT/TP_010deg_prec.rda")
    lst_prec %<>% do.call(cbind, .) %>% FUN_tidy
    
    load("INPUT/TP_010deg_srad.rda")
    lst_srad %<>% do.call(cbind, .) %>% FUN_tidy
    
    dates <- seq(ymd('19820101'), ymd('20151231'), by = "day")
    info_date <- data.table(date = dates, year = year(dates), yday = yday(dates))
    
    varnames = .(Tmin, Tmax, Prec, Srad) %>% names()
    ngrid <- nrow(gridclip2_10)
    
    r <- foreach(Tmin = lst_temp$Tmin, 
        Tmax = lst_temp$Tmax,
        Prec = lst_prec, 
        Srad = lst_srad,
        d_sos = df_SOS_10deg[I_rem, ] %>% t(), 
        d_eos = df_EOS_10deg[I_rem, ] %>% t(), 
        I_eos = d_EOS_avg[I_rem] %>% floor(), 
        i = icount(), 
        .packages = c("magrittr", "ppcor", "data.table", "foreach", "matrixStats"), 
        .export = c()) %dopar% {
            phenofit::runningId(i, 100, ngrid)
        df = data.table(Tmin, Tmax, Prec, Srad) %>% set_names(c("Tmin", "Tmax", "Prec", "Srad")) %>% 
            cbind(info_date, .)
        
        d_pheno <- data.table(SOS = d_sos[, 1], EOS = d_eos[, 1])
        mat_pcor <- foreach(j = 1:10, .combine = "rbind") %do% {
            I_start <- I_eos - 15*j + 1
            d <- df[yday <= I_eos & yday >= I_start, lapply(.SD, mean, na.rm = T), 
                    .(year), .SDcols=varnames] %>% .[, -1] %>% cbind(d_pheno)
            I_nona <- is.na(d) %>% rowSums2(na.rm=TRUE) %>% {which(. == 0)}
            d <- d[I_nona, ]
            res <- pcor(d)$estimate
            res <- res[-nrow(res), "EOS"]
        } %>% set_rownames(NULL)
        pcor_max <- mat_pcor %>% apply(2, which_max)
        
        d <- foreach(var = varnames, pos = pcor_max[2, ], .combine = "cbind") %do% {
            # print(pos)
            I_start <- I_eos - 15*pos + 1
            d <- df[yday <= I_eos & yday >= I_start, lapply(.SD, mean, na.rm = T), 
                    .(year), .SDcols=var] %>% .[, -1]
            d
        } %>% cbind(d_pheno)
        # return
        list(pcor.max = pcor_max[1, ], preseason = pcor_max[2, ], data = d)
    } 
    mat_preseason <- r %>% purrr::transpose() # 
    mat_preseason[1:2] %<>% map(~do.call(rbind, .))
    save(mat_preseason, file = "OUTPUT/TP_010deg_preseason.rda")
    
    ## 2. Autumn phenology model -----------------------------------------------
    load("data/00basement_TP.rda")
    load('OUTPUT/phenology_TP_AVHRR_phenofit_010deg.rda')
    load("OUTPUT/TP_010deg_preseason.rda")
    
    I_rem <- which(!(is.na(d_SOS_avg) | is.na(d_EOS_avg))) # about 2/3
    gridclip2_10 <- gridclip_10[I_rem, ]
    ngrid <- length(gridclip2_10)
    
    # 2.1 max_pcor
    gridclip2_10@data <- as.data.frame(mat_preseason$pcor.max)
    brks <- critical_pcor(32, 3) %>% c(1) %>% c(-rev(.), 0, .) %>% round(3)

    g <- spplot_grid(gridclip2_10, colors = colors$corr, brks = brks, layout = c(2, 3), 
        pars = list(
            title = list(x=76.5, y=39, cex=1.5),
            hist = list(origin.x=77, origin.y=27, A=15, by = 0.5, axis.x.text = FALSE, ylab.offset = 2.5)))
    write_fig(g, "Figure_s2_max_pcor.pdf", 10, 7.1)
    
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
    InitCluster(6)
    lst_pls <- foreach(d = mat_preseason$data, i = icount(),.combine = , 
                       .packages = c("magrittr", "phenology")) %dopar% {
            phenofit::runningId(i, 100, ngrid)
            I_nona <- is.na(d) %>% matrixStats::rowSums2(na.rm=TRUE) %>% {which(. == 0)}
            d <- d[I_nona, ] %>% as.matrix()
            
            X <- d[, 1:5] # METE + SOS
            Y <- d[, 6, drop=FALSE]   # EOS 
            
            plsreg1_adj(X, Y, comps = 2, autoVars = TRUE, nminVar = 2, minVIP = 0.8)
            # m_opls <- opls(X %>% as.matrix(), Y %>% as.matrix(), predI = 2, plotL = FALSE, printL=FALSE) # 32 times slower 
        } %>% 
        purrr::transpose() # %>% map(~do.call(rbind, .))
    
    tidy_init <- . %>% map(~.[1, -1]) %>% do.call(rbind, .)
    tidy_last <- . %>% map(~.[2, -1]) %>% do.call(rbind, .)

    pls_init <- map(lst_pls, tidy_init)
    pls_last <- map(lst_pls, tidy_last)
    
    save(pls_init, pls_last, file = "OUTPUT/TP_010deg_pls_preseason_OUTPUT.rda")

    ## 2.4 The difference of considering SOS or not
    InitCluster(6)
    lst_pls <- foreach(d = mat_preseason$data, 
                       i = icount(),.combine = , 
                       .packages = c("magrittr", "phenology")) %dopar% {
            phenofit::runningId(i, 100, ngrid)
            I      <- 1:nrow(d)
            I_nona <- is.na(d) %>% matrixStats::rowSums2(na.rm=TRUE) %>% {which(. == 0)}
            d <- d[I_nona, ] %>% as.matrix()
            d <- d %>% as.matrix()
            
            X <- d[, 1:5]             # METE + SOS
            Y <- d[, 6, drop=FALSE]   # EOS 
            
            m <-plsreg1_adj(X, Y, comps = 2, autoVars = FALSE, include.fitted = TRUE)$init %>% 
                plsr_fix_ypred(I, I_nona)
            # drop SOS
            m_nonSOS <-plsreg1_adj(X[, -5], Y, comps = 2, autoVars = FALSE, include.fitted = TRUE)$init %>% 
                plsr_fix_ypred(I, I_nona)

            list(SOS = m, nonSOS = m_nonSOS)
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
