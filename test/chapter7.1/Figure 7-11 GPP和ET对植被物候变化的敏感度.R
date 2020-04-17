setwd("/mnt/n/Research/phenology/phenologyTP")
# print(getwd())

devtools::load_all()
source("test/main_pkgs.R")

## -----------------------------------------------------------------------------
file_LAI <- "data-raw/lst_LAI.rda"
load(file_LAI)
load(file_PML)
lst_pheno <- readRDS(file_pheno)
years_gpp <- 2003:2017
# load("data-raw/pheno_trend.2000_2015.rda")

# %% ---------------------------------------------------------------------------
grid <- grid_010.TP_cliped2
grid_010.TP@data <- data.frame(id = 1:length(grid_010.TP))
ind_full <- raster::extract(raster(grid_010.TP), grid_010.TP_cliped)
ind_lcMask <- grid$id_cliped

lst_LAI2 <- map_depth(lst_LAI, 2, ~do.call(cbind, .))

nrow <- length(ind_lcMask)
indexes <- 1:nrow %>% set_names(., .)

{
    InitCluster(12, kill = TRUE)
    str_FUN <- "coef_lm"
    # str_FUN <- "coef_pls"
    
    FUN <- get(str_FUN)
    temp <- foreach(l = lst_pheno[5:7], i = icount()) %do% {
        info    <- match2(l$year, years_gpp)
        l_pheno <- map(l[c(1,3)] %>% rm_empty, ~.[, info$I_x])

        SOS <- l_pheno$SOS
        EOS <- l_pheno$EOS
        
        l_PML <- map(df_dynamic, ~.[ind_full, info$I_y][ind_lcMask, ])
        ET <- abind(l_PML[-1], along = 3) %>% apply_3d(FUN = rowSums2)
        Y <- c(list(ET = ET), l_PML)[c(2, 1, 3, 4, 5)] # add Ei
        
        l_LAI <- map_depth(lst_LAI2, 2, ~.x[, info$I_y])
        res = foreach(LAI = l_LAI) %do% {
            X = c(list(SOS = SOS[ind_lcMask, ], EOS = EOS[ind_lcMask, ]), LAI)
            foreach(k = seq_along(Y) %>% set_names(names(Y))) %dopar% {
                lst_data <- c(Y[k], X)
                ans <- foreach(i = indexes, icount()) %dopar%
                    {
                        runningId(i, 1000)
                        d <- map(lst_data, ~ .x[i, ]) %>% as.data.table() %>% na.omit()
                        if (nrow(d) <= 6) return(NULL)
                        FUN(d)
                    } %>% rm_empty()
            }    
        }
        
        # V1-V4: c("SOS", "EOS", "gsMean", "yearMax")
        l_coef   <- map_depth(res, 3, "coef") %>% map_depth(2, melt_cbind)
        l_pvalue <- map_depth(res, 3, "pvalue") %>% map_depth(2, melt_cbind)
        list(coef = l_coef, pvalue = l_pvalue)
    }
    
    lst_res <- transpose(temp) %>% map(
        function(l) {
            ans <- melt_tree(l, c("type_source", "type_LAI", "response"))
            colnames(ans)[5:8] <- c("SOS", "EOS", "gsMean", "yearMax")
            ans
        }
    )
    outfile <- glue("chp7_dynamic-static_pheno-{str_FUN}.RDS")
    saveRDS(lst_res, outfile)
}

# grid@data <- l_PML$GPP %>% as.data.table()
# plot(grid)

# save(lst_pcor, file = "chp7_dynamic-static_GPP&ET_pcor.rda")
# load("chp7_dynamic-static_GPP&ET_pcor.rda")

# lst <-map(lst_pcor, ~melt(.x, id.vars = c("type_source", "type_LAI", "response", "I")))
# df <- lst$pcor %>% cbind(pvalue = lst$pvalue$value) %>% plyr::mutate(mask = pvalue <= 0.1)
# df$variable %<>% as.character() %>% factor(c("SOS", "EOS", "yearMax", "gsMean"), 
#                                           c("生长季开始时间", "生长季结束时间", "年LAI最大值", "生长季LAI均值"))
# df$response %<>% factor(c("GPP", "ET", "Ec", "Es"), c("GPP", "ET", "Ec", "Es"))
# ## 2.1 raw LAI -----------------------------------------------------------------
# {
#     df2 <- df[type_source %in% sources[5:7] & type_LAI == "raw", ] %>% 
#         plyr::mutate(type_source = factor(type_source, sources[5:7]))
#     devices = c("jpg", "pdf")[2]
#     SpatialPixel <- grid_010.TP_cliped2
#     plot_pcor_spatial2(df2, "GPP", SpatialPixel, devices, TRUE)
#     plot_pcor_spatial2(df2, "ET", SpatialPixel, devices, TRUE)
#     plot_pcor_spatial2(df2, "Ec", SpatialPixel, devices, TRUE)
#     plot_pcor_spatial2(df2, "Es", SpatialPixel, devices, TRUE)
    
#     d_id <- map(lst_id[-c(7, 10)], ~data.table(I = .x)) %>% melt_list("region")
    
#     data <- merge(df2, d_id)
#     cor    <- data[is.finite(value), mean(value), .(type_source, response, variable, region)] %>% dcast2("variable", "V1")
#     d_sign <- data[is.finite(value), sign_perc(value, mask), .(type_source, response, variable, region)] 
#     pos <- dcast2(d_sign[, -6], "variable", "pos")
#     neg <- dcast2(d_sign[, -5], "variable", "neg")
    
#     write_list2xlsx(list(cor, pos, neg), "tbl_7-5 dynamic-static pcor3.xlsx")
# }

# ## 2.2 smoothed LAI ------------------------------------------------------------
# {
#     df2 <- df[type_source %in% sources[5:7] & type_LAI == "smoothed", ] %>% 
#     plyr::mutate(type_source = factor(type_source, sources[5:7]))
#     devices = c("jpg", "pdf")[2]
#     prefix = "smoothed_"
#     SpatialPixel <- grid_010.TP_cliped2

#     plot_pcor_spatial2(df2, "GPP", SpatialPixel, devices, TRUE, prefix)
#     plot_pcor_spatial2(df2, "ET", SpatialPixel, devices, TRUE, prefix)
#     plot_pcor_spatial2(df2, "Ec", SpatialPixel, devices, TRUE, prefix)
#     plot_pcor_spatial2(df2, "Es", SpatialPixel, devices, TRUE, prefix)
# }

# grid <- grid_010.TP_cliped2
# overlap_id(grid, TP_poly_veg)
