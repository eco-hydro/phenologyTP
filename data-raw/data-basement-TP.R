load('data/00basement_TP.rda')

grid_010.TP <- gridclip_10
grid_012.TP_cliped <- gridclip

grid_010.TP_cliped <- grid_avhrr # 16156 pixels

plot(grid_012.TP_cliped)
plot(grid_010.TP)

use_data(grid_010.TP, overwrite = TRUE)
use_data(grid_012.TP_cliped, overwrite = TRUE)
use_data(grid_010.TP_cliped, overwrite = TRUE)

TP_poly <- poly
TP_poly_veg <- poly_veg
use_data(TP_poly, overwrite = TRUE)
use_data(TP_poly_veg, overwrite = TRUE)
