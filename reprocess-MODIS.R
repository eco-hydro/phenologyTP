library(ncdf4)
library(rgdal)
library(h5)
library(gdalUtils)

file <- "N:/MODIS_NDVI/MOD13C1/MOD13C1.A2000049.006.2015147153445.hdf"
sds  <- get_subdatasets(file)

library(R.matlab)
file <- "N:/MODIS_NDVI/MOD13C1/NDVI.mat"
x <- readMat(file)

write_fig({
    image(x$NDVI)
}, "a.png")
