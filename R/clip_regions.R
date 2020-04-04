clip_region <- function(shp, obj_global) {
    bbox <- shp@bbox
    cellsize <- obj_global$cellsize[1]
    range = c(floor(bbox[,1]/cellsize)*cellsize, ceiling(bbox[,2]/cellsize)*cellsize)[c(1,3,2,4)]

    obj_basin <- clip_rect(obj_global, range, cliped.obj=TRUE)

    # str(obj_basin)
    # ids_inglobal <- obj_basin$ids
    grid <- obj_basin$grid
    # sp::plot(shp, add = TRUE)
    # ids0 <- raster::extract(raster(grid), shp)[[1]]
    grid_full <- as(grid, "SpatialPolygonsDataFrame")
    grid_cliped <- raster::intersect(grid_full, shp)

    # plot(grid_full)
    # plot(shp, add = TRUE, border = "red")
    ids <- grid_cliped@data$id
    I = match(ids, grid$id)
    area <- area(grid_cliped) / 1e6 # km^2
    fraction <- area(grid_cliped) / area(grid_full[I, ])
    info = data.table(ids, area, fraction)
    list(info, grid, shp_grid)
}
