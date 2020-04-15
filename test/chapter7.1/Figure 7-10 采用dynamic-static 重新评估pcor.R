source("test/main_pkgs.R")

read_tiff <- function(files){
    lst <- map(files, ~ readGDAL(.)@data[, 1][id] / 10)
    ind <- 1:15
    years = 2003:2017
    lst_raw <- lst[ind] %>% set_names(years)
    lst_smoothed <- lst[-ind] %>% set_names(years)
    list(raw = lst_raw, smoothed = lst_smoothed)
}

file_LAI = "data-raw/lst_LAI.rda"
if (!file.exists(file_LAI)) {
    grid <- grid_010.TP_cliped2
    id <- grid_010.TP_cliped2$id

    files <- dir("INPUT/Annual_LAI_max", full.names = TRUE)
    lst_yearMax = read_tiff(files)

    files <- dir("INPUT/Annual_gs_mean", full.names = TRUE)
    lst_gsMean = read_tiff(files)

    lst_LAI <- list(gsMean = lst_gsMean, yearMax = lst_yearMax) %>% transpose()
    save(lst_LAI, file = file_LAI)
} else {
    load(file_LAI)
}

# lst2 <- map(files, ~readGDAL(.)@data[, 1][id]/10)
# lst_gs_raw <- 
# plot(grid_010.TP_cliped2)
# mat <- x@data[, 1][id]
# grid_010.TP_cliped2@data <- data.table(x = mat)
# grid_010.TP_cliped2@data <- data.table(x = mat)

## -----------------------------------------------------------------------------
load(file_PML)
# load("data-raw/pheno_trend.2000_2015.rda")

# %% ---------------------------------------------------------------------------
lst_pheno <- readRDS(file_pheno)
years_gpp <- 2003:2017

grid <- grid_010.TP_cliped2
grid_010.TP@data <- data.frame(id = 1:length(grid_010.TP))
ind_full <- raster::extract(raster(grid_010.TP), grid_010.TP_cliped)
ind_lcMask <- grid$id_cliped

lst_LAI2 <- map_depth(lst_LAI, 2, ~do.call(cbind, .))

nrow <- length(ind_lcMask)
indexes <- 1:nrow %>% set_names(., .)

{
    InitCluster(12)
    temp <- foreach(l = lst_pheno, i = icount()) %do% {
        info <- match2(l$year, years_gpp)
        l_pheno <- map(l[c(1,3)] %>% rm_empty, ~.[, info$I_x])

        SOS <- l_pheno$SOS
        EOS <- l_pheno$EOS
        l_PML <- map(df_dynamic, ~.[ind_full, info$I_y][ind_lcMask, ])
        ET <- abind(l_PML[-1], along = 3) %>% apply_3d(FUN = rowSums2)
        Y <- c(list(ET = ET), l_PML)[c(2, 1, 3, 4, 5)]
            
        l_LAI <- map_depth(lst_LAI2, 2, ~.x[, info$I_y])
        res = foreach(LAI = l_LAI) %do% {
            X = c(list(SOS = SOS[ind_lcMask, ], EOS = EOS[ind_lcMask, ]), LAI)
            foreach(k = seq_along(Y) %>% set_names(names(Y))) %do% {
                l <- c(Y[k], X)
                ans <- foreach(i = indexes, icount()) %dopar%
                    {
                        runningId(1000)
                        d <- map(l, ~ .x[i, ]) %>% as.data.table()
                        pcor2(d)
                        # xs <- map(1:nrow, function(i) map(l, ~.x[i, ]) %>% as.data.table)
                    } %>% rm_empty()
            }    
        }
        l_pcor   <- map_depth(res, 3, "estimate") %>% map_depth(2, melt_cbind)
        l_pvalue <- map_depth(res, 3, "p.value") %>% map_depth(2, melt_cbind)
        list(pcor = l_pcor, pvalue = l_pvalue)
    }
}

# grid@data <- l_PML$GPP %>% as.data.table()
# plot(grid)
lst_pcor <- transpose(temp) %>% map(
    function(l) melt_tree(l, c("type_source", "type_LAI", "response"))
)
save(lst_pcor, file = "chp7_dynamic-static_GPP&ET_pcor.rda")
load("chp7_dynamic-static_GPP&ET_pcor.rda")

lst <-map(lst_pcor, ~melt(.x, id.vars = c("type_source", "type_LAI", "response", "I")))
df <- lst$pcor %>% cbind(pvalue = lst$pvalue$value) %>% plyr::mutate(mask = pvalue <= 0.1)
df$variable %<>% as.character() %>% factor(c("SOS", "EOS", "yearMax", "gsMean"), 
                                          c("生长季开始时间", "生长季结束时间", "年LAI最大值", "生长季LAI均值"))
df$response %<>% factor(c("GPP", "ET", "Ec", "Es"), c("GPP", "ET", "Ec", "Es"))
## 2.1 raw LAI -----------------------------------------------------------------
{
    df2 <- df[type_source %in% sources[5:7] & type_LAI == "raw", ] %>% 
        plyr::mutate(type_source = factor(type_source, sources[5:7]))
    devices = c("jpg", "pdf")[2]
    SpatialPixel <- grid_010.TP_cliped2
    plot_pcor_spatial2(df2, "GPP", SpatialPixel, devices, TRUE)
    plot_pcor_spatial2(df2, "ET", SpatialPixel, devices, TRUE)
    plot_pcor_spatial2(df2, "Ec", SpatialPixel, devices, TRUE)
    plot_pcor_spatial2(df2, "Es", SpatialPixel, devices, TRUE)
    
    d_id <- map(lst_id[-c(7, 10)], ~data.table(I = .x)) %>% melt_list("region")
    
    data <- merge(df2, d_id)
    cor    <- data[is.finite(value), mean(value), .(type_source, response, variable, region)] %>% dcast2("variable", "V1")
    d_sign <- data[is.finite(value), sign_perc(value, mask), .(type_source, response, variable, region)] 
    pos <- dcast2(d_sign[, -6], "variable", "pos")
    neg <- dcast2(d_sign[, -5], "variable", "neg")
    
    write_list2xlsx(list(cor, pos, neg), "tbl_7-5 dynamic-static pcor3.xlsx")
}

## 2.2 smoothed LAI ------------------------------------------------------------
{
    df2 <- df[type_source %in% sources[5:7] & type_LAI == "smoothed", ] %>% 
    plyr::mutate(type_source = factor(type_source, sources[5:7]))
    devices = c("jpg", "pdf")[2]
    prefix = "smoothed_"
    SpatialPixel <- grid_010.TP_cliped2

    plot_pcor_spatial2(df2, "GPP", SpatialPixel, devices, TRUE, prefix)
    plot_pcor_spatial2(df2, "ET", SpatialPixel, devices, TRUE, prefix)
    plot_pcor_spatial2(df2, "Ec", SpatialPixel, devices, TRUE, prefix)
    plot_pcor_spatial2(df2, "Es", SpatialPixel, devices, TRUE, prefix)
}

grid <- grid_010.TP_cliped2
overlap_id(grid, TP_poly_veg)
