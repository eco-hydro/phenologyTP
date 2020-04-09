
grid_010.TP_cliped@data <- data.table(id = 1:nrow(grid_010.TP_cliped))
d <- poly_veg@data
r <- raster::extract(raster(grid_010.TP_cliped), poly_veg) %>% set_names(d$Name)
id_veg_010deg <- r %>% map(rm_empty)
use_data(id_veg_010deg, overwrite = TRUE)
