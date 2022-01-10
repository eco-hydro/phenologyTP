# grouped into: Forest, Shrubland, Grassland, Cropland (CNV and CRO), Urban and Water and others
source("test/main_pkgs.R")
library(rPML)

{
    shp <- readOGR("data-raw/shp/world_poly.shp")
    sp_layout <- list("sp.polygons", shp, lwd = 0.2, first = FALSE)
    
    poly <- readOGR("data-raw/ArcGIS/shp/representative_poly.shp", verbose = FALSE)
    sp_poly <- list("sp.polygons", poly, first = FALSE, col = "black")
}

grid5  <- get_grid(range_global, cellsize = 0.5, type = "vec")

file_PML_static  = "data-raw/PML-V2_static-2003-2017_G05deg.nc"
file_PML_dynamic = "data-raw/PML-V2_dynamic-2003-2017_G05deg.nc"

rewrite = FALSE
if (rewrite) {
    indir = "INPUT/tif/v016/"
    files_dynamic <- dir(indir, "PMLV2_veg-dynamic", full.names = TRUE) %>%
        set_year_names()
    
    files_static  <- dir(indir, "PMLV2_veg-static", full.names = TRUE) %>%
        set_year_names()
    
    lst_dynamic <- llply(files_dynamic, readGDAL, band = 1:4)
    lst_static  <- llply(files_static , readGDAL, band = 1:4)
    
    bandNames = c("GPP", "Ec", "Es", "Ei", "ET_water")
    lst_dynamic2 <- tidy_PML(lst_dynamic, grid_global)
    lst_static2  <- tidy_PML(lst_static, grid_global)
    
    lst_static2$ET  <- abind(lst_static2[2:4], along = 3) %>% apply_3d(FUN = rowSums2, na.rm = FALSE)
    lst_dynamic2$ET <- abind(lst_dynamic2[2:4], along = 3) %>% apply_3d(FUN = rowSums2, na.rm = FALSE)

    # r <- spdata_array(lst_static2$GPP, )
    range = c(-180, 180, -60, 90)
    dates = seq(ymd("2003-01-01"), ymd("2017-01-01"), by = "year")
    
    l <- lst_static2 %>% map(spdata_array, range, cellsize = 0.5)
    ncwrite(l, file_PML_static, range = range, dates = dates)
    l <- lst_dynamic2 %>% map(spdata_array, range, cellsize = 0.5)
    ncwrite(l, file_PML_dynamic, range = range, dates = dates)
} else {
    lst_dynamic2 <- ncread(file_PML_dynamic, -1, grid_type = "vec")$data
    lst_static2 <- ncread(file_PML_static, -1, grid_type = "vec")$data
}

# file = files_dynamic[1]
# x = readGDAL(file)
# r = brick(x)
# raster::plot(raster::as.raster(x))

# Figure 5. The spatial pattern of (a) mean annual GPPrs and (b) mean annual ETrs,
# averaged for 2004-2017, and LUCC-driven mean annual change in (c) GPPrs and (d)
# ETrs from the two PML-V2 experiments (Dynamic – Static, in Eq. (1)).

# mean annual change during 2004-2018
df_diff <- map2(lst_dynamic2, lst_static2, ~rowMeans2(.x - .y, na.rm = TRUE))[-1] %>% 
    do.call(cbind, .) %>% data.table() %>%
    add_column_id() %>% melt("I", variable.name = "band")
df_diff[value ==0, value := 0]

bands_en = c("ET", "Ec", "Es", "Ei")
bands_zh = c("蒸散发 (mm)", "植被蒸腾 (mm)", "土壤蒸发 (mm)", "冠层截留蒸发 (mm)")
df_diff$band %<>% factor(bands_en, bands_zh)
# df_diff$band %<>% mapvalues(bands_en, bands_zh)
    
{
    load_all("../lattice.layers")
    max = 4
    # brks <-  c(-Inf, seq(-2, 2, 0.2), Inf) # perc
    # brks <-  c(-Inf, seq(-max, max, 0.4), Inf) # km^2, 3600 in total
    # cols <- colorRampPalette(c("red", "white", "green"))(ncol)
    stat = list(show = FALSE, name = "RC", loc = c(80, 26.5), digit = 1, include.sd =
                    FALSE, FUN = weightedMedian)
    pars = list(title = list(x = -180, y = 90, cex = 1.4))
    
    bandNames <- c("ET", "Ec", "Es", "Ei")
    # ps = foreach(bandName = bandNames[1:4], i = icount()) %do% {
        # if (i == 1) {
        #     brks <- {c(5, 10, 20, 50, 100, 200)} %>% c(-Inf, -rev(.), 0, ., Inf)
        #     ncol <- length(brks) - 1
        #     cols <- get_color("MPL_RdYlGn", ncol) #%>% rev()
        # } else {
            brks <- {c(2, 5, 10, 20, 50)} %>% c(-Inf, -rev(.), 0, ., Inf)
            ncol <- length(brks) - 1
            cols <- get_color("amwg256", ncol) %>% rev()
        # }
        # d <- df_diff[band == bandName]
        # d %<>% plyr::mutate(lev = cut(value, brks))
        # tbl_perc <- na.omit(d$lev) %>% table() %>% {./sum(.)*100}
        p <- levelplot2(value ~ s1+s2 | band,
                    # df,
                    df_diff,
                    # df[!(LC %in% c("UNC", "water"))], # blank
                    # df[LC %in% IGBP006_names[1:4]],
                    grid5,
                    # df.mask = df[, .(LC, mask = pval <= 0.05)],
                    colors = cols, brks = brks,
                    # layout = c(2, 2),
                    pars = pars,
                    yticks = seq(0, 0.2, 0.1),
                    ylim = c(-72, 97),
                    xlim = c(-190, 240),
                    aspect = 0.5,
                    # unit = "km2", unit.adj = 0.5,
                    legend.num2factor = TRUE,
                    colorkey = list(width = 1.4, height = 0.96, labels = list(cex = 1)),
                    sp.layout = list(sp_layout, sp_poly),
                    # par.settings2 = list(
                    #     xlab.key.padding = 0,
                    #     axis.line = list(col = "white"))
                    interpolate = FALSE
                    # stat = NULL,
                    # xlim = xlim, ylim = ylim
        ) +
            theme_lattice(key.margin = c(0, 1, 0, 0),
                          plot.margin = c(0, 1, -1.5, 0))
        # tbl_perc
    # }
    # tbl2 <- do.call(rbind, ps) %>% data.table() %>% cbind(band = bandNames[1:4], .)
    # write_list2xlsx(list(tbl2 = tbl2), "dat2_LUCC_induced x changes.xlsx")
    # g = arrangeGrob(grobs = ps, nrow = 2)
    write_fig(p, "Figure2_GPP_ET_dynamic-static (2004-2017)_rep_poly_20200430.pdf", 11.1, 5.2)
}


# {
#     # select representative poly in ArcGIS
#     grid5@data <- dcast(df_diff, I ~ band, value.var = "value")[, -1]
#     r <- brick(grid5)
#     plot(r[2])

#     Ec = subset(r, 2) #%>% plot()
#     writeRaster(Ec, "delta_EC_2004-2017.tif")
#     # m <- leaflet() %>% addTiles() %>%
#     #     addRasterImage(r, colors = pal, opacity = 0.8) %>%
#     #     addLegend(pal = pal, values = values(r),
#     #               title = "Surface temp")
# }
