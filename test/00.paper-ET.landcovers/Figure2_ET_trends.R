# grouped into: Forest, Shrubland, Grassland, Cropland (CNV and CRO), Urban and Water and others
source("test/main_pkgs.R")

{
    # shp <- readOGR("data-raw/shp/world_poly.shp")
    shp <- readOGR("data-raw/shp/continent.shp")
    sp_layout <- list("sp.polygons", shp, lwd = 0.5, first = FALSE)
    
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

# Figure 5. The spatial pattern of (a) mean annual GPPrs and (b) mean annual ETrs,
# averaged for 2004-2017, and LUCC-driven mean annual change in (c) GPPrs and (d)
# ETrs from the two PML-V2 experiments (Dynamic – Static, in Eq. (1)).

bands_en = c("ET", "Ec", "Es", "Ei")
bands_zh = c("蒸散发 (mm)", "植被蒸腾 (mm)", "土壤蒸发 (mm)", "冠层截留蒸发 (mm)")
bands_zh_perc = c("蒸散发 (%)", "植被蒸腾 (%)", "土壤蒸发 (%)", "冠层截留蒸发 (%)")

d_mean  <- lst_dynamic2[-1] %>% map(rowMeans2) %>% 
    do.call(cbind, .) %>% data.table()
d_perc  <- {d_mean[, 1:3]/d_mean$ET*100} %>% 
    add_column_id() %>% melt("I", variable.name = "band") %>% {
        .$band %<>% factor(bands_en[-1], bands_zh_perc[-1]); . }

df_mean <- lst_dynamic2[-1] %>% map(rowMeans2) %>% 
    do.call(cbind, .) %>% data.table() %>%
    add_column_id() %>% melt("I", variable.name = "band") %>% {
        .$band %<>% factor(bands_en, bands_zh); . } %>%
    data.table()

# mean annual change during 2004-2018
df_diff <- map2(lst_dynamic2, lst_static2, ~rowMeans2(.x - .y, na.rm = TRUE))[-1] %>% 
    do.call(cbind, .) %>% data.table() %>%
    add_column_id() %>% melt("I", variable.name = "band")  %>% {
        .$band %<>% factor(bands_en, bands_zh); . } %>% 
    data.table()
df_diff[value == 0, value := 0]


stat = list(show = FALSE)
pars = list(title = list(x = -180, y = 90, cex = 1.4))

df <- merge(df_mean[, .(I, band, mean = value)], df_diff, sort = FALSE)
df %<>% mutate(perc = value/mean*100)

Figure2 = TRUE
if (Figure2) {
    # load_all("../latticeGrob")
    set_options(list(style = "CH"))
    # get_options() %>% str()
    stat = list(show = TRUE, name = "u", loc = c(-30, -60), digit = 1, include.sd = TRUE, FUN = matrixStats::weightedMean)
    
    brks <- c(-Inf, 10, 20, 50, 100, 150, 200, 300, 400, 500, 800, 1000, 1500, Inf)
    # brks <- {c(2, 5, 10, 20, 50)} %>% c(-Inf, -rev(.), 0, ., Inf)
    ncol <- length(brks) - 1
    cols <- get_color("amwg256", ncol) %>% rev()
    
    bar <- list("SpatialPolygonsRescale", layout.scale.bar(height=0.1), 
                offset = c(40, -40), scale = 45, fill=c("white", "black"), first = FALSE)
    delta = 12
    l1 = list("sp.text", c(40, -40+delta), "0", fontfamily = "Times")
    l2 = list("sp.text", c(90, -40+delta), "5000 km", fontfamily = "Times")
    
    p <- levelplot2(mean ~ s1+s2 | band,
                    df,
                    grid5,
                    colors = cols, brks = brks,
                    layout = c(2, 2),
                    pars = pars,
                    stat = stat, 
                    unit = "(mm)", unit.adj = 0.5,
                    yticks = seq(0, 0.2, 0.1),
                    ylim = c(-72, 99),
                    xlim = c(-190, 190),
                    aspect = 0.5,
                    show_signPerc = FALSE,
                    prob_z = 0.98, 
                    bbox_barchartFreq = c(0.05, 0.26, 0.15, 0.4),
                    # show_horizontalFreq = FALSE, 
                    zlim_ratio = c(0, 1),
                    # unit = "km2", unit.adj = 0.5,
                    legend.num2factor = TRUE,
                    colorkey = list(width = 1.4, height = 0.96, labels = list(cex = 1)),
                    sp.layout = list(bar, l1, l2), # , sp_poly, sp_layout
                    interpolate = FALSE
    ) + 
        theme_lattice(key.margin = c(0, 1.5, 0, 0),
                      plot.margin = c(0, 3, -1.5, 1))
    write_fig(p, "Figure2_ET_multi-annual average (2003-2017).pdf", 11.4, 5.2)
}
# {c(268.6, 303.2, 161.4, 103.4, 46.6, 77.2)/476.7*100} %>% round(1)

# lat = 0; rdist.earth(matrix(c(0, lat), nrow = 1), matrix(c(67.4, lat), nrow = 1), miles = FALSE)
lon = 0; rdist.earth(matrix(c(0, 0), nrow = 1), matrix(c(0, 45), nrow = 1), miles = FALSE)


Figure3 = TRUE
if (Figure3) {
    # set_options(list(style = "CH"))
    # load_all("../latticeGrob")
    brks <- {c(2, 5, 10, 20, 50)} %>% c(-Inf, -rev(.), 0, ., Inf)
    ncol <- length(brks) - 1
    cols <- get_color("amwg256", ncol) %>% rev()
    
    p <- levelplot2(value ~ s1+s2 | band,
        df_diff,
        grid5,
        colors = cols, brks = brks,
        # layout = c(2, 2),
        pars = pars,
        stat = stat,
        yticks = seq(0, 0.2, 0.1),
        ylim = c(-72, 99),
        xlim = c(-190, 190),
        aspect = 0.5,
        unit = "(mm)", unit.adj = 0.5,
        legend.num2factor = TRUE,
        show_horizontalFreq = FALSE,
        colorkey = list(width = 1.4, height = 0.96, labels = list(cex = 1)),
        sp.layout = list(bar, l1, l2, sp_poly),
        interpolate = FALSE
    ) + 
        theme_lattice(key.margin = c(0, 1.5, 0, 0),
                      plot.margin = c(0, 3, -1.5, 1))
    write_fig(p, "Figure3_GPP_ET_dynamic-static (2004-2017)_rep_poly_20200430.pdf", 11.4, 5.2)
}

Figure4 = TRUE
if (Figure4) {
    # load_all("../latticeGrob")
    
    brks <- {c(2, 5, 10, 15, 20)} %>% c(-Inf, -rev(.), 0, ., Inf)
    ncol <- length(brks) - 1
    cols <- get_color("amwg256", ncol) %>% rev()
    
    p <- levelplot2(perc ~ s1+s2 | band,
                    df,
                    # df[band == bands_zh[2], ],
                    grid5,
                    colors = cols, brks = brks,
                    panel.titles_full = bands_zh_perc %>% label_tag(),
                    # layout = c(2, 2),
                    pars = pars,
                    stat = stat,
                    yticks = seq(0, 0.2, 0.1),
                    ylim = c(-72, 99),
                    xlim = c(-190, 190),
                    aspect = 0.5,
                    unit = "(%)", unit.adj = 0.5,
                    legend.num2factor = TRUE,
                    colorkey = list(width = 1.4, height = 0.96, labels = list(cex = 1)),
                    sp.layout = list(bar, l1, l2, sp_poly),
                    interpolate = FALSE
    ) + 
        theme_lattice(key.margin = c(0, 1.5, 0, 0),
                      plot.margin = c(0, 3, -1.5, 1))
    write_fig(p, "Figure4_ET_dynamic-static change ratio (2004-2017).pdf", 11.4, 5.2)
}

# 三层蒸发的比例
Figure5 = TRUE
if (Figure5) {
    # load_all("../latticeGrob")
    
    brks <- {c(2, 5, seq(10, 90, 10), 95)} %>% c(-Inf, ., Inf)
    ncol <- length(brks) - 1
    cols <- get_color("amwg256", ncol) %>% rev()
    stat = list(show = TRUE, name = "u", loc = c(-30, -60), digit = 1, include.sd = TRUE, FUN = matrixStats::weightedMean)
    
    p <- levelplot2(value ~ s1+s2 | band,
                    d_perc,
                    # d_perc[band == bands_zh_perc[4], ],
                    grid5,
                    colors = cols, brks = brks,
                    panel.titles_full = bands_zh_perc[-1] %>% label_tag(),
                    # layout = c(2, 2),
                    pars = pars,
                    stat = stat,
                    unit = "(%)", unit.adj = 0.5,
                    yticks = seq(0, 0.2, 0.1),
                    ylim = c(-72, 99),
                    xlim = c(-190, 240),
                    aspect = 0.5,
                    show_signPerc = FALSE, 
                    prob_z = 0.98, 
                    # unit = "km2", unit.adj = 0.5,
                    legend.num2factor = TRUE,
                    colorkey = list(width = 1.4, height = 0.96, labels = list(cex = 1)),
                    sp.layout = list(sp_layout, sp_poly),
                    interpolate = FALSE
    ) + 
        theme_lattice(key.margin = c(0, 1.5, 0, 0),
                      plot.margin = c(0, 3, -1.5, 1))
    write_fig(p, "Figure5_ET component ratio (2003-2017).pdf", 11.3, 5.2)
}
