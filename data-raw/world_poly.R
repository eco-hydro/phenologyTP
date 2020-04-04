## code to prepare `DATASET` dataset goes here

write_shp = FALSE
if (write_shp) {
    library(rnaturalearth)
    library(rnaturalearthdata)
    # large
    world <- ne_countries(scale = "medium", returnclass = "sp")
    # writeOGR(world[, 1], "data-raw/world_poly.shp", "world_poly", driver = "ESRI Shapefile", overwrite_layer = TRUE)

    # simplified in ArcGIS
}

world <- readOGR("data-raw/shp/world_poly.shp", verbose = FALSE)
usethis::use_data(world, overwrite = TRUE)


# 检查LAI数据缺失
d = fread("data-raw/imgcol_lai.txt")
d$date %<>% ymd()

diff(d$date) %>% table()
seq_date_dn('2002-07-04', '2019-02-01') %>% setdiff(d$date) %>% as.Date("1970-01-01")
# [1] "2002-08-01" "2003-12-19" "2016-02-22" "2018-07-28"
