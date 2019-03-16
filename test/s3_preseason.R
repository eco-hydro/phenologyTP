library(Rcmip5)
library(Ipaper)
library(foreach)
library(iterators)

which_max <- function(x) {
    I <- which.max(abs(x))
    c(value = x[I], pos = I)
}

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
    mat_preseason <- r %>% purrr::transpose() # %>% map(~do.call(rbind, .))
    save(mat_preseason, file = "OUTPUT/TP_010deg_preseason.rda")
    
    # 2. prepare input
    gridclip2_10@data <- as.data.frame(mat_preseason$value)
    brks <- critical_pcor(32, 3) %>% c(1) %>% c(-rev(.), 0, .) %>% round(3)

    g <- spplot_grid(gridclip2_10, colors = colors$corr, brks = brks, layout = c(2, 3), 
        pars = list(
            title = list(x=76.5, y=39, cex=1.5),
            hist = list(origin.x=77, origin.y=27, A=15, by = 0.5, axis.x.text = FALSE, ylab.offset = 2.5)))
    write_fig(g, "Figure_s2_max_pcor.pdf", 10, 7.1)
}
