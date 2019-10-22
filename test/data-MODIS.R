source("test/main_pkgs.R")
library(R.matlab)
load("data/00basement_TP.rda") 
load(file_pheno_010)

library(raster)
# library()

file_MOD <- "E:/Research/phenology/phenofit.shiny/INPUT/TP_MOD13C1_020deg_grid (200002-201909).mat"
l <- readMat(file_MOD)

range = c(73, 105, 25, 40)

r <- get_grid2(range, cellsize = 1/20)
grid <- as(r, "SpatialPixelsDataFrame")
# y <- over(x, raster(gridclip2_10))

mask = raster(gridclip2_10)
mask_poly = rasterToPolygons(mask)
poly   = as(gridclip2_10, "SpatialPolygonsDataFrame")

lst_inds = extract(r, poly) 
I_mod = unlist(lst_inds) %>% unique() #%>% length()

NDVI = l$mat.NDVI[,,] %>% aperm(c(2, 1, 3)) %>% array_3dTo2d(I_mod)
QC   = l$mat.QC[,,] %>% aperm(c(2, 1, 3)) %>% array_3dTo2d(I_mod)
gridclip <- grid[I_mod, ]

save(NDVI, QC, gridclip, I_mod, file = "E:/Research/phenology/phenofit.shiny/INPUT/TP_MOD13C1_020deg_pixel (200002-201909).rda")

{
    # lon at first, and image(mat %>% flipup())
    # x <- t(mat) %>% as.numeric()
    # grid@data <- data.frame(x)
    # plot(grid)
    gridclip@data <- as.data.frame(NDVI[, 1:4])
    # df = as.data.frame(mat[, 1:4])
    # grid@data <- df
    write_fig(expression({
        spplot(gridclip) %>% print()
    }), "a.tif", 10, 6)
}
