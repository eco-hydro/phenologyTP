## Figure 7-03 -----------------------------------------------------------------
# PMLV2 动态与静态模拟
if (!file.exists(file_PML)){
    indir = "INPUT/tif/V016/"
    files_dynamic <- dir(indir, "veg-dynamic", full.names = TRUE) %>%
        set_year_names()
    files_static  <- dir(indir, "veg-static", full.names = TRUE) %>%
        set_year_names()
    files_lai <- dir("INPUT/tif/", "LAI", full.names = TRUE) %>%
        set_year_names()
    files_lc <- dir("INPUT/tif/", "landcover_perc_G010", full.names = TRUE) %>%
        set_year_names()

    lst_dynamic <- llply(files_dynamic, readGDAL, band = 1:4)
    lst_static  <- llply(files_static , readGDAL, band = 1:4)
    lst_lai     <- llply(files_lai, readGDAL)
    # grouped into: Forest, Shrubland, Grassland, Cropland (CNV and CRO), Urban and Water and others
    bandNames = c("GPP", "Ec", "Es", "Ei", "ET_water")
    
    tidy_PML2 <- function(lst, id) {
        lst %>% map(~.x@data[id, ] %>% set_names(bandNames[1:4])) %>%
            # resample_lst(., grid) %>%
            transpose() %>% map(~do.call(cbind, .))
    }
    grid <- grid_010.TP
    # grid <- grid_010.TP_cliped
    id_grid_010.TP_in_global <- raster::extract(raster(grid_global), grid)
    lst_dynamic <- tidy_PML2(lst_dynamic, id_grid_010.TP_in_global)
    lst_static  <- tidy_PML2(lst_static, id_grid_010.TP_in_global)
    mat_LAI <- lst_lai %>% map(~.x@data[id_grid_010.TP_in_global, ]) %>% do.call(cbind, .)
                
    save(lst_dynamic, lst_static, mat_LAI, file = file_PML)
} else {
    load(file_PML)
}

## Figure 7-10 -----------------------------------------------------------------
# 偏相关分析素材：
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
    
    mat <- lst_yearMax$raw %>% do.call(cbind, .) %>% rowMeans2()
    save(lst_LAI, file = file_LAI)
} else {
    load(file_LAI)
}

