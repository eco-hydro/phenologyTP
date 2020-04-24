# grouped into: Forest, Shrubland, Grassland, Cropland (CNV and CRO), Urban and Water and others
source("test/main_pkgs.R")
add_ETsum <- function(d) {
    d_ET = d[band != "GPP", .(band = "ET", value = sum(value, na.rm = TRUE)), .(I)]
    rbind(d, d_ET)
}

{
    grid_full <- get_grid(range = c(73, 105, 25, 40), cellsize=0.1, type = "vec")
    ind_TP <- raster::extract(raster(grid_full), grid_010.TP)
    bands <- c("GPP", "ET", "Ec", "Es", "Ei")
    lst_dynamic <- ncread("INPUT/PML_V2-yearly-TP_010deg (2003-2017) veg_dynamic.nc", -1, convertTo2d = TRUE, grid_type = "vec")$data %>% 
        .[c(1, 2, 10, 3)] %>% set_names(bands[-2]) %>% 
        map(~.[ind_TP, ])
    lst_static <- ncread("INPUT/PML_V2-yearly-TP_010deg (2003-2017) veg_static.nc", -1, convertTo2d = TRUE, grid_type = "vec")$data %>% 
        .[c(1, 2, 10, 3)] %>% set_names(bands[-2]) %>% 
        map(~.[ind_TP, ])
}

## -----------------------------------------------------------------------------
load(file_PML)

# mean annual change during 2004-2018
df_diff2 <- foreach(mat_d = lst_dynamic, mat_s = lst_static, i = icount()) %do% {
    diff = mat_d - mat_s
    rowMeans2(diff, na.rm = TRUE)
} %>% do.call(cbind, .) %>% data.table() %>%
    add_column_id() %>% melt("I", variable.name = "band") %>% 
    add_ETsum()
df_diff2[value == 0, value := 0]

grid <- grid_010.TP
df_mean <- lst_dynamic %>% map(rowMeans2) %>% as.data.table() %>% 
    cbind(I = 1:nrow(grid), .) %>% melt("I", variable.name = "band") %>% 
    add_ETsum()

# grid_full <- get_grid(range = c(73, 105, 25, 40), cellsize = 0.1, type = "vec")
# ind2 <- raster::extract(raster(grid_full), grid_010.TP)
# l <- l_diff[c(1, 11, 2, 10, 3)] %>% map(~rowMeans2(.x)) %>% as.data.table()
# grid2 <- grid_010.TP
# df2 <- l[ind2, ]
# grid2@data <- df2

# poly <- readOGR("data-raw/ArcGIS/shp/representative_poly.shp", verbose = FALSE)
# sp_poly <- list("sp.polygons", poly, first = FALSE, col = "black")
{
    max = 4
    stat = list(show = FALSE, name = "RC", loc = c(80, 26.5), digit = 1, include.sd =
                    FALSE, FUN = weightedMedian)
    # hist = list(origin.x = -150,
    #             origin.y = -20, A = 90, by = 5,
    #             tck = 2,
    #             ylab.offset = 25)
    # pars = list(title = list(x = -180, y = 85, cex = 1.4),
    #             hist = hist)
    hist = list(origin.x = -150,
                origin.y = -20, A = 90, by = 5,
                tck = 2,
                ylab.offset = 25)
    pars = list(title = list(x=77, y=39, cex=1.5),
                hist = list(origin.x=77, origin.y=28, A=15, by = 0.4))
    
    bandNames = c("GPP", "ET", "Ec", "Es", "Ei")
    bandNames_zh = c(expression("总初级生产力 (g C" ~ m^-2 * a^-1 * ")"), 
                     "蒸散发 (mm)", "植被蒸腾 (mm)", "土壤蒸发 (mm)", "顶冠截流 (mm)")
    names <- label_tag(bandNames_zh)
    names[1] <- expression("(a) 总初级生产力 (g C" ~ m^-2 * a^-1 * ")")
    names %<>% char2expr()
}


id_veg_010deg <- overlap_id(grid_010.TP, poly_veg)
res <- foreach(ind = id_veg_010deg) %do% {
    df_mean[I %in% ind, .(value = mean(value, na.rm = TRUE)), .(band)]
}
d <- res %>% melt_list("veg_type") %>% dcast(veg_type~band)

## Figure 7-3 spatial distribution of simulations ------------------------------
{
    ps = foreach(bandName = bandNames, i = icount()) %do% {
        if (i == 1) {
            brks <- {c(1, 5, 10, 20, 50, 100, 200, 500, 1000)} %>% c(-Inf, ., Inf)
            ncol <- length(brks) - 1
            cols <- get_color("MPL_RdYlGn", ncol) #%>% rev()
        } else {
            brks <- {c(1, 5, 10, 20, 50, 100, 200, 300, 400, 500, 600)} %>% c(-Inf, ., Inf)
            ncol <- length(brks) - 1
            cols <- get_color("amwg256", ncol) %>% rev()
        }

        d <- df_mean[band == bandName]
        d %<>% plyr::mutate(lev = cut(value, brks))
        tbl_perc <- na.omit(d$lev) %>% table() %>% {./sum(.)*100}

        p <- levelplot2(value ~ s1+s2 | band,
                    # df,
                    d,
                    # df[!(LC %in% c("UNC", "water"))], # blank
                    # df[LC %in% IGBP006_names[1:4]],
                    grid,
                    # df.mask = df[, .(LC, mask = pval <= 0.05)],
                    colors = cols, brks = brks,
                    # layout = c(2, 2),
                    pars = pars,
                    # ylim = c(-60, 92),
                    strip = FALSE,
                    ylim = c(25, 40),
                    xlim = c(73, 105),
                    par.settings2 = list(strip.background = list(alpha = 0), axis.line = list(col = "transparent")),
                    par.strip.text = list(cex = 1.5, font = 2, fontfamily = "Times", lineheight = 2),
                    aspect = 0.55,
                    panel.title = names[i],
                    # unit = "km2", unit.adj = 0.5,
                    sub.hist = TRUE,
                    legend.num2factor = TRUE,
                    colorkey = list(width = 1.4, height = 0.92, labels = list(cex = 1)),
                    sp.layout = list(sp_layout),
                    # par.settings2 = list(
                    #     xlab.key.padding = 0,
                    #     axis.line = list(col = "white"))
                    interpolate = FALSE
                    # stat = NULL,
                    # xlim = xlim, ylim = ylim
        ) +
            theme_lattice(key.margin = c(0, 1, 0, 0),
                          plot.margin = c(0.5, 0.5, -1.5, -1.5))
    }
    # "no applicable method for 'as.layer' applied to an object of class "list""
    # tbl2 <- do.call(rbind, ps) %>% data.table() %>% cbind(band = bandNames[1:4], .)
    # write_list2xlsx(list(tbl2 = tbl2), "dat2_LUCC_induced x changes.xlsx")
    g = arrangeGrob(grobs = ps, nrow = 3)
    write_fig(g, "Figure7-3 GPP_ET spatial dist (2003-2017).jpg", 9.4, 7)
}

# Figure4: dynamic - static ----------------------------------------------------
{
    ps = foreach(bandName = bandNames, i = icount()) %do% {
        if (i == 1) {
            brks <- {c(5, 10, 20, 50, 100)} %>% c(-Inf, -rev(.), 0, ., Inf)
            ncol <- length(brks) - 1
            cols <- get_color("MPL_RdYlGn", ncol) #%>% rev()
        } else {
            brks <- {c(1, 2, 5, 10, 20, 50)} %>% c(-Inf, -rev(.), 0, ., Inf)
            ncol <- length(brks) - 1
            cols <- get_color("amwg256", ncol) %>% rev()
        }

        d <- df_diff2[band == bandName]
        # d <- df2[, ..i] %>% set_colnames("value") %>% cbind(band = bandName, .)
        # d %<>% plyr::mutate(lev = cut(value, brks))
        # tbl_perc <- na.omit(d$lev) %>% table() %>% {./sum(.)*100}

        p <- levelplot2(value ~ s1+s2 | band,
                    # df,
                    d,
                    # df[!(LC %in% c("UNC", "water"))], # blank
                    # df[LC %in% IGBP006_names[1:4]],
                    grid,
                    # df.mask = df[, .(LC, mask = pval <= 0.05)],
                    colors = cols, brks = brks,
                    # layout = c(2, 2),
                    pars = pars,
                    # ylim = c(-60, 92),
                    strip = FALSE,
                    ylim = c(25, 40),
                    xlim = c(73, 105),
                    par.settings2 = list(strip.background = list(alpha = 0), axis.line = list(col = "white")),
                    par.strip.text = list(cex = 1.5, font = 2, fontfamily = "Times", lineheight = 2),
                    aspect = 0.55,
                    panel.title = names[i],
                    # unit = "km2", unit.adj = 0.5,
                    sub.hist = TRUE,
                    legend.num2factor = TRUE,
                    colorkey = list(width = 1.4, height = 0.92, labels = list(cex = 1)),
                    sp.layout = list(sp_layout),
                    # par.settings2 = list(
                    #     xlab.key.padding = 0,
                    #     axis.line = list(col = "white"))
                    interpolate = FALSE
                    # stat = NULL,
                    # xlim = xlim, ylim = ylim
        ) +
            theme_lattice(key.margin = c(0, 1, 0, 0),
                          plot.margin = c(0.5, 0.5, -1.5, -1.5))
        # tbl_perc
    }
    # tbl2 <- do.call(rbind, ps) %>% data.table() %>% cbind(band = bandNames[1:4], .)
    # write_list2xlsx(list(tbl2 = tbl2), "dat2_LUCC_induced x changes.xlsx")
    g = arrangeGrob(grobs = ps, nrow = 3)
    write_fig(g, "Figure7-4 GPP_ET_dynamic-static (2004-2017) V2.jpg", 9.4, 7)
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
