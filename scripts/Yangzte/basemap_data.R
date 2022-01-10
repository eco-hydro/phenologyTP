# source("test/00.paper-ET.landcovers/basemap_data.R")
source("test/main_pkgs.R")

{
    # shp <- readOGR("data-raw/shp/world_poly.shp")
    shp <- readOGR("data-raw/shp/basin_长江.shp")
    sp_region <- list("sp.polygons", shp, lwd = 0.5, 
                      first = F)
    # poly <- readOGR("data-raw/ArcGIS/shp/representative_poly.shp", verbose = FALSE)
    # sp_poly <- list("sp.polygons", poly, first = FALSE, col = "black")
}

grid5 <- get_grid(range_global, cellsize = 0.1, type = "vec")

get_river <- function(file, lwd = 0.5) {
    shp_river = read_sf(file)
    st_crs(shp_river) = 4326
    shp_river = st_intersection(shp_river, st_as_sf(shp))
    sp_river <- list("sp.polygons", as_Spatial(shp_river), 
                     lwd = lwd, fill = "skyblue", col = "blue", first = FALSE)
    sp_river    
}
sp_river_L2 = get_river("G:/ArcGIS_common/中国地图2008/国家基础地理信息系统数据/hyd2_4p.shp", lwd = 0.2)
sp_river_L1 = get_river("G:/ArcGIS_common/中国地图2008/国家基础地理信息系统数据/一级河流/hyd1_4l.shp", lwd = 0.5)
