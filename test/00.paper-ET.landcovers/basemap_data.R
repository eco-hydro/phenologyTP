# source("test/00.paper-ET.landcovers/basemap_data.R")
source("test/main_pkgs.R")

{
    # shp <- readOGR("data-raw/shp/world_poly.shp")
    shp <- readOGR("data-raw/shp/continent.shp")
    sp_continent <- list("sp.polygons", shp, lwd = 0.5, first = FALSE)

    poly <- readOGR("data-raw/ArcGIS/shp/representative_poly.shp", verbose = FALSE)
    sp_poly <- list("sp.polygons", poly, first = FALSE, col = "black")
}
grid5 <- get_grid(range_global, cellsize = 0.5, type = "vec")
