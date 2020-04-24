# grouped into: Forest, Shrubland, Grassland, Cropland (CNV and CRO), Urban and Water and others
source("test/main_pkgs.R")

indir = "INPUT/tif/v015/"
files_dynamic <- dir(indir, "PML2_yearly_dynamic", full.names = TRUE) %>%
    set_year_names()

files_static  <- dir(indir, "PML2_yearly_static", full.names = TRUE) %>%
    set_year_names()

lst_dynamic <- llply(files_dynamic, readGDAL, band = 1:4)
lst_static  <- llply(files_static , readGDAL, band = 1:4)

bandNames = c("GPP", "Ec", "Es", "Ei", "ET_water")
lst_dynamic <- tidy_PML(lst_dynamic, grid)
lst_static  <- tidy_PML(lst_static, grid)

file = files_dynamic[1]
x = readGDAL(file)
# r = brick(x)
# raster::plot(raster::as.raster(x))

# Figure 5. The spatial pattern of (a) mean annual GPPrs and (b) mean annual ETrs,
# averaged for 2004-2017, and LUCC-driven mean annual change in (c) GPPrs and (d)
# ETrs from the two PML-V2 experiments (Dynamic – Static, in Eq. (1)).

# mean annual change during 2004-2018
df_diff <- foreach(mat_d = lst_dynamic, mat_s = lst_static, i = icount()) %do% {
    diff = mat_d - mat_s
    rowMeans2(diff, na.rm = TRUE)
} %>% do.call(cbind, .) %>% data.table() %>%
    add_column_id() %>% melt("I", variable.name = "band")
df_diff[value ==0, value := 0]
{
    poly <- readOGR("data-raw/ArcGIS/shp/representative_poly.shp", verbose = FALSE)
    sp_poly <- list("sp.polygons", poly, first = FALSE, col = "black")
    max = 4
    # brks <-  c(-Inf, seq(-2, 2, 0.2), Inf) # perc
    # brks <-  c(-Inf, seq(-max, max, 0.4), Inf) # km^2, 3600 in total
    # cols <- colorRampPalette(c("red", "white", "green"))(ncol)
    #
    stat = list(show = FALSE, name = "RC", loc = c(80, 26.5), digit = 1, include.sd =
                    FALSE, FUN = weightedMedian)
    hist = list(origin.x = -150,
                origin.y = -20, A = 90, by = 5,
                tck = 2,
                ylab.offset = 25)
    pars = list(title = list(x = -180, y = 85, cex = 1.4),
                hist = hist)

    ps = foreach(bandName = bandNames[1:4], i = icount()) %do% {
        if (i == 1) {
            brks <- {c(5, 10, 20, 50, 100, 200)} %>% c(-Inf, -rev(.), 0, ., Inf)
            ncol <- length(brks) - 1
            cols <- get_color("MPL_RdYlGn", ncol) #%>% rev()
        } else {
            brks <- {c(1, 5, 10, 20, 50, 100)} %>% c(-Inf, -rev(.), 0, ., Inf)
            ncol <- length(brks) - 1
            cols <- get_color("amwg256", ncol) %>% rev()
        }

        d <- df_diff[band == bandName]
        d %<>% plyr::mutate(lev = cut(value, brks))
        tbl_perc <- na.omit(d$lev) %>% table() %>% {./sum(.)*100}
        p <- levelplot2(value ~ s1+s2 | band,
                    # df,
                    d,
                    # df[!(LC %in% c("UNC", "water"))], # blank
                    # df[LC %in% IGBP006_names[1:4]],
                    grid5,
                    # df.mask = df[, .(LC, mask = pval <= 0.05)],
                    colors = cols, brks = brks,
                    # layout = c(2, 2),
                    pars = pars,
                    ylim = c(-60, 92),
                    aspect = 0.5,
                    # unit = "km2", unit.adj = 0.5,
                    sub.hist = TRUE,
                    legend.num2factor = TRUE,
                    colorkey = list(width = 1.4, height = 0.98, labels = list(cex = 1)),
                    sp.layout = list(sp_layout, sp_poly),
                    # par.settings2 = list(
                    #     xlab.key.padding = 0,
                    #     axis.line = list(col = "white"))
                    interpolate = FALSE
                    # stat = NULL,
                    # xlim = xlim, ylim = ylim
        ) +
            theme_lattice(key.margin = c(0, 1.5, 0, 0),
                          plot.margin = c(0, 3, -1.5, 0))
        # tbl_perc
    }
    # tbl2 <- do.call(rbind, ps) %>% data.table() %>% cbind(band = bandNames[1:4], .)
    # write_list2xlsx(list(tbl2 = tbl2), "dat2_LUCC_induced x changes.xlsx")
    g = arrangeGrob(grobs = ps, nrow = 2)
    write_fig(g, "Figure2_GPP_ET_dynamic-static (2004-2017)_rep_poly_20190121.pdf", 10, 4.4)
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
