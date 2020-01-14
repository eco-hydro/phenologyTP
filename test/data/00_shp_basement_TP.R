# files <- dir("G:/Github/data", "*.nc", full.names = T)
# fid   <- nc_open(files[1])
# I_grid <- fid$dim$Id$vals
library(sp)

I_grid <- data.table::fread("data/Id_avhrr_TP.txt")$V1

baesdata <- function(outfile = 'data/00basement_TP.rda'){
    # spatial grid info
    range  <- c(25, 40, 73, 105) # Tibetan Plateau
    grid   <- Ipaper::get_grid(range, cellsize = 1/12, midgrid = TRUE)
    gridclip <- grid[I_grid, ]
    gridclip$id <- 1:nrow(gridclip)
    # poly_grid  <- as(gridclip, "SpatialPolygonsDataFrame")

    # GET TP boundary
    region <- "mongo" # "TP" # 
    shpfile_tp <- "../data/shp/TP/TP_vegZoneSolve84.shp"
    shpfile_mongo <- "../data/shp/mongo/veg_mongo84.shp"

    poly_veg    <- rgdal::readOGR(shpfile_tp, verbose = FALSE)
    sp_layout <- list("sp.polygons", poly_veg, first = FALSE)
    
    # p_mongo <- rgdal::readOGR(shpfile_mongo, verbose = FALSE)

    bbox <- sp::bbox(poly_veg)
    xlim <- bbox[1, ] + c(0, 0.3)
    ylim <- bbox[2, ] + c(0, 0.3)

    ## selected agro sites, 1:28 mongo
    st <- data.table::fread("data/st_TPmongo_rep107.csv", encoding = "UTF-8") #%>% df2sp() 
    st[, `:=`(longname = sprintf("[%03d]_%d_%s", 1:.N, site, name), 
              Id = 1:.N)]
    sp <- st %>% df2sp()

    id <- over(sp, gridclip)$id
    I_rem <- which(!is.na(id))
    I_st <- id[I_rem]

    st <- st[I_rem, ]
    sp <- sp[I_rem, ]

    cellsize <- 0.1
    grid_10 <- get_grid(range, cellsize)

    proj4string(poly_veg) <- proj4string(grid)
    I_grid_10 <- over(grid_10, poly_veg[, 1])[[1]] %>% which.notna()
    gridclip_10 <- grid_10[I_grid_10, ]

    save(gridclip, I_grid, 
         gridclip_10, I_grid_10, 
         range,
         poly_veg, sp_layout, st, sp, I_st, xlim, ylim, 
         file = outfile)
    res <- listk(gridclip, poly_veg, I_grid, I_st, st, sp, xlim, ylim)
    res
}

res <- baesdata()


## 统计alpine steppe的比例
library(raster)
library(sp)
r = over( gridclip_10, poly_veg[, 2])


# label_style <- 
#     labelOptions(
#         noHide = T, textOnly = TRUE,
#         style=list(
#             'color'='red',
#             'font-family'= 'Calibri',
#             'font-style'= 'italic',
#             # 'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
#             'font-size' = '12px'
#             # 'border-color' = 'rgba(0,0,0,0.5)'
#         ))

# basemap(gridclip, poly_grid, p_TP) %>% 
#     # addTiles() %>% 
#     addCircleMarkers(data = sp_TPmongo, label = ~as.character(site), popup = ~info, 
#                labelOptions = label_style)
