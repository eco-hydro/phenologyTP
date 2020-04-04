load("INPUT/lc_trends.rda")
# visualization
# grid2 <- as(grid, "SpatialPixelsDataFrame")
range = c(-180, 180, -60, 90)
grid  <- get_grid(range, 0.1, type = "vec")
grid5 <- get_grid(range, 0.5, type = "vec")

# grid2@data <- l$trend %>% as.data.frame()
df_trend <- resample_lst(lc_trends, grid)
df <- melt_list(df_trend, "LC") %>% data.table() %>%
    .[slp == 0, slp := NA_real_]
df[, area := slp*3600] # km^2
df$LC %<>% factor(IGBP006_names)

xlim = range[1:2]
ylim = range[3:4]

{
    library(rnaturalearth)
    library(rnaturalearthdata)
    world <- ne_countries(scale = "medium", returnclass = "sp")
    sp_layout <- list("sp.polygons", world, first = FALSE, lwd = 0.2) # , fill = "transparent"
    # class(world)
}
{
    max = 3600
    brks <-  c(-Inf, seq(-2, 2, 0.2), Inf) # perc
    brks <-  c(-Inf, seq(-max, max, 400), Inf) # km^2, 3600 in total

    ncol <- length(brks) - 1
    cols <- colorRampPalette(c("red", "white", "green"))(ncol*2)
    # cols <- get_color("GMT_red2green", ncol*2) %>% rev()

    stat = list(show = FALSE, name = "RC", loc = c(81.5, 26.5), digit = 1, include.sd =
                    FALSE, FUN = weightedMedian)
    hist = list(origin.x = -120,
         origin.y = 10, A = 60, by = 2)
    pars = list(title = list(x = -180, y = 80, cex = 1.2),
                hist = hist)
    p <- levelplot2(area ~ s1+s2 | LC,
                    df,
                    # df[LC %in% IGBP006_names[1:4]],
                    grid5,
                    # df.mask = df[, .(LC, mask = pval <= 0.05)],
                    colors = cols, brks = brks,
                    layout = c(3, 6),
                    pars = pars,
                    # unit = "km2", unit.adj = 0.5,
                    sub.hist = TRUE,
                    sp.layout = sp_layout,
                    interpolate = FALSE
                    # stat = NULL,
                    # xlim = xlim, ylim = ylim
                    ) +
        theme_lattice(key.margin = c(1, 1.5, 0, 0),
                      plot.margin = c(0, 4, 0, 1))
    write_fig(p, "Figure1_LC_changes_16IGBPs.png", 12, 10)
}

# 0.1 deg area: 24*24 * 0.25 (km2) #(240/10 = 24)
# 0.5 deg area: (24*24 * 0.25 * 5^2) = 3600 #km2
