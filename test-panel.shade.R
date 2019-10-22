
r <- gridclip2_10[, 1]
d <- r@data %>% data.table(x = .)

I_shade = which(d$x >= 260)
poly_shade = as(r[I_shade, ], "SpatialPolygonsDataFrame")

{
    # 进一步区分，正的显著和负的显著
    p <- spplot(gridclip2_10, 1, 
                density = 1, col = "black", 
                lwd = 1.2, 
                poly_shade = poly_shade,
                panel = panel.gridplot2)
    write_fig(p,"shade.tif", 10, 6)    
}

## test about panel.title
p <- spplot(gridclip2_10, 1)

title.top  = grid.text("top")
title.left = grid.text("left")

library(grid)
pushViewport(viewport(layout=grid.layout(2, 2)))
