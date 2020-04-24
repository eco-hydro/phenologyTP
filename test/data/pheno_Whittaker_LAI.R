source("test/main_pkgs.R")

# 采用Elmore logistics对植被指数进行重构，定量的控制生长季的开始时间和结束时间的变化
files = dir("INPUT/phenofit_INPUT", "TP.tif", full.names = TRUE) %>% 
    set_names(c("MCD15A2H_smoothed", "MCD15A2H", "MOD15A2H", "MOD17A2H_GPP"))
nptperyears = c(rep(2, 2), 1, 1) * 46

# infile <- "INPUT/phenofit_INPUT/MCD15A2H_smoothed_LAI_010deg_TP.tif"
# file_band <- gsub(".tif$", "_names.csv", infile)
grid <- grid_010.TP
grid_full <- get_grid(range = c(73, 105, 25, 40), cellsize = 0.1, type = "vec")
ind_TP <- raster::extract(raster(grid_full), grid)

# ------------------------------------------------------------------------------
InitCluster(12)
# lst <- foreach(infile = files[3], i = icount()) %do% {
    infile = files[3]
    file_band <- gsub(".tif$", "_names.csv", infile)
    dates <- get_date_FromBandnames(file_band)
    tout <- get_modis_date(dates)
    
    I_time <- match(tout, dates)
    # info <- data.table(date = dates, year = year(dates))[, .N, .(year)]

    lst <- readGDAL(infile)
    data <- as.matrix(lst@data/10) %>% 
        .[ind_TP, I_time] %>% 
        set_colnames(as.character(tout))
    res <- process_ExtractPheno(data, tout, return.fit = TRUE)
# }

res2 <- res %>% rm_empty()
ind <- names(res2) %>% as.numeric()

## convert into matrix ---------------------------------------------------------
varnames <- 1:13 %>% as.character() %>% set_names(., .)
meths <- c("Beck", "Elmore") %>% set_names(., .)
lst_res <- foreach(method = meths) %do% {
    foreach(varname = varnames, j = icount()) %do% {
        runningId(j)
        l <- list()
        for (i in seq_along(res2)) {
            l[[i]] <- res2[[i]][meth == method, ..varname][[1]]
        }
        mat <- do.call(rbind, l)
    } 
}

outfile <- "OUTPUT/LAI phenofit smoothed TP_010deg.nc"
lst_mat <- foreach(meth = meths, i = icount()) %do% {
    runningId(i)
    mat <- abind(lst_res[[meth]], along = 3) %>%
        aperm(c(1, 3, 2)) %>%
        clamp(c(0, 10))
    # -32 - 32
}

outfile <- "OUTPUT/LAI phenofit smoothed TP_010deg_V2.nc"
lst_mat <- ncread("OUTPUT/LAI phenofit smoothed TP_010deg.nc", -1)$data

ncwrite(lst_mat[1:2], outfile,
        prec = "short", scale = rep(0.001, 2),
        range = NULL,
        dates = tout, verbose = TRUE, overwrite = TRUE)

ncwrite(list(grid_id = lst_mat$grid_id), outfile,
        prec = "short",
        # scale = rep(0.001, 2),
        range = NULL,
        dates = tout,
        verbose = TRUE, overwrite = FALSE)

# l <- ncread(outfile, -1, ntime = 2)$data
#  
# mat_diff <- l$Beck - l$Elmore
# as.numeric(mat_diff) %>% summary()

# df_LAI <- transpose(rm_empty(res)) %>% melt_tree(c("meth", "I"))
# df_LAI$I %<>% as.numeric()
# saveRDS(df_LAI, file = "data-raw/pheno_smoothed_LAI (2003-2017).RDS")

### statistics in nc files
## Elmore
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.135   0.210   0.428   0.424  10.000

## Beck
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.135   0.210   0.428   0.424  10.000

# l <- map_depth(lst_res, 2, ~clamp(.x, c(0, 10))*100 %>% as.integer())
# df <- rm_empty(res) %>% melt_list('I')

# {
#     I_time <- which(dates >= '2003-01-01' & dates <= "2019-12-31")
#     dates <- dates[I_time]
#     years = year(dates)
#     grps = unique(years)
#     # only kept the complete year
# }

# data <- lst@data
# id   <- grid$id
# {
#     grid2 <- 
#     grid2 <- grid2[id, ]
#     plot(grid2)    
#     grid2@data <- lst@data[id, ]
# }

# r <- lst[, 1]
# plot(r[id, ])
# load("data-raw/lst_LAI.rda")
# vec <- lst_LAI$raw$gsMean %>% do.call(cbind, .) %>% rowMeans2()
# plot(data[100, ], type = "l")

# {
#     grid <- grid_010.TP_cliped2
#     d <- df_LAI[, lapply(.SD, mean, na.rm = TRUE), .(I), .SDcols = colnames(df_LAI)[-(1:4)]]
#     ngrid <- length(grid)
#     d <- expand.grid(I = 1:ngrid) %>% data.table() %>% merge(d, all.x = TRUE)
#     grid@data <- d[, -1]
# }

# brks <- .brks$SOS
# cols <- get_color2(.colors$SOS, length(brks)-1)
# spplot(grid, metric_spring, at = brks, col.regions = cols, as.table = TRUE)

# # 在羌塘高原提取的植被物候信息是错误的 -----------------------------------------
# brks <- .brks$EOS
# cols <- get_color2(.colors$EOS, length(brks)-1)
# spplot(grid, metric_autumn, at = brks, col.regions = cols, as.table = TRUE)

## lai应该保留小数点后两位数字 -------------------------------------------------

# grid.draw(p)
# tout = 1:365
# fFITs <- curvefit(y, t, tout, methods = "Beck")
# PhenoTrs(fFITs$fFIT$Beck)
# INPUT <- check_input()
