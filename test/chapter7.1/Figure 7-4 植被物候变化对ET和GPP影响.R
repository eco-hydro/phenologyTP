source("test/main_pkgs.R")

load(file_PML)
load("data/pheno_trend.2000_2015.rda")
## 1. 准备一个0.1deg的高程文件
d_elv <- readGDAL("E:/github/hydro/VICtools/inst/database/ELEV_srtm_010deg_china.tif") %>% as_SpatialPixelsDataFrame()
{
    grid <- grid_010.TP
    dem <- raster::extract(raster(d_elv), grid_010.TP)
    grid@data <- data.table(dem)
    dem <- grid
    spplot(dem)
    use_data(dem, overwrite = TRUE)
    write_tiff(dem, "data-raw/ELEV_srtm_010deg_TP.tif")
}

## 2. 植被植被物候的逐年文件 (2003-2015)


