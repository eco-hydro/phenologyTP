# load("INPUT/lc_trends.rda")
# visualization
source("test/main_pkgs.R")

# obj_global <- make_grid(range=c(-180, 180, -60, 90), cellsize=0.1)
# shp <- TP_poly
# grid <- obj_global$grid[obj$ids, ]
# system.time(obj <- sp2:::clipbasins(shp, obj_global))
# PML result in 0.1 deg
# id_grid_010.TP_in_global <- raster::extract(raster(grid), grid_010.TP)
use_data(id_grid_010.TP_in_global, overwrite = TRUE)

# v2: cliped
# grid <- grid_010.TP
grid <- grid_010.TP_cliped
id_grid_010.TP_in_global <- raster::extract(raster(grid_global), grid)

# grid5 <- get_grid(range_global, cellsize = 0.5)
d_diff       <- fread("INPUT/lc_diff (2008-2017)-(2003-2007).csv")
# d_diff       <- fread("INPUT/lc_diff (2008-2018)-(2003-2007).csv")
# d_diff       <- fread("INPUT/lc_diff (2018)-(2008-2017).csv")

## clip regional data
{
    d_diff_major <- aggregate_lc(d_diff[id_grid_010.TP_in_global, ] %>% as.matrix)[, ]
    d_major <- melt_lc(d_diff_major)
    
    r <- grid_010.TP
    # r@data <- d[, -1] 
    # df = resample_grid(grid, d_diff, fact = 5)@data %>% add_column_id() %>% melt('I', variable.name = "LC")
    # df_major = resample_grid(grid, d_diff_major, fact = 1)@data %>% data.table() %>% melt_lc()
}

{
    sp_layout2 <- c(list(sp_layout), list(list("sp.polygons", TP_poly, fill = alpha("white", 0.5), first = TRUE)))
    max = 4
    # brks <-  c(-Inf, seq(-2, 2, 0.2), Inf) # perc
    # brks <-  c(-Inf, seq(-max, max, 0.4), Inf) # km^2, 3600 in total
    brks <- c(0.1, 0.2, 0.5, 1, 2, 5) %>% c(-Inf, -rev(.), 0, ., Inf)
    ncol <- length(brks) - 1
    cols <- colorRampPalette(c("red", "white", "green"))(ncol)
    # cols <- get_color("GMT_red2green", ncol*2) %>% rev()

    stat = list(show = FALSE, name = "RC", loc = c(80, 26.5), digit = 1, include.sd =
                    FALSE, FUN = weightedMedian)
    hist = list(origin.x = -150,
                origin.y = -20, A = 90, by = 5,
                tck = 2,
                ylab.offset = 25)
    pars = list(title = list(x=77, y=39, cex=1.5),
                hist = list(origin.x=77, origin.y=28, A=15, by = 0.4))
    # stat = list(show = TRUE, name="mean", loc = c(82.5, 26.5), digit = 1, include.sd = TRUE)

    # par.strip.text = list(cex = 1.5, font = 2, fontfamily = "Times", lineheight = 2),
    # # par.settings = opt_trellis_strip,
    # interpolate = FALSE,
    # aspect = .6)
    levels = c("森林", "灌木", "草地", "耕地", "城市", "水体")
    p <- levelplot2(value ~ s1+s2 | LC,
                    # df,
                    d_major,
                    # df[!(LC %in% c("UNC", "water"))], # blank
                    # df[LC %in% IGBP006_names[1:4]],
                    grid,
                    # df.mask = df[, .(LC, mask = pval <= 0.05)],
                    colors = cols, brks = brks,
                    strip = TRUE,
                    layout = c(2, 3),
                    pars = pars,
                    aspect = 0.5,
                    ylim = c(25, 40),
                    strip.factors = levels, 
                    xlim = c(73, 105),
                    par.settings2 = list(strip.background = list(alpha = 0)),
                    par.strip.text = list(cex = 1.5, font = 2, fontfamily = "Times", lineheight = 2),
                    # unit = "km2", unit.adj = 0.5,
                    sub.hist = TRUE,
                    legend.num2factor = TRUE,
                    sp.layout = sp_layout2,
                    interpolate = FALSE
                    # stat = NULL,
                    # xlim = xlim, ylim = ylim
    ) +
        theme_lattice(key.margin = c(1, 1.5, 0, 0),
                      plot.margin = c(0, 4, -1, 1))
    # p %<>% useOuterStrips(
    #     # strip = strip.custom(factor.levels = labels),
    #                       strip.lines = 1.1)
    write_fig(p, "Figure1_LC_changes_major_(2008-2017)-(2003-2007)_cliped.jpg", 9, 7.2)
}

# d <- na.omit(df_major)
# d %<>% plyr::mutate(lev = cut(value, brks))
# tbl1_count <- d[, as.list(table(lev)), .(LC)]
# tbl1_perc  <- d[, as.list(table(lev) %>% {./sum(.)*100}), .(LC)]

# write.xlsx(tbl1_perc, "dat1_LC_change_ratio.xlsx")
# 0.1 deg area: 24*24 * 0.25 (km2) #(240/10 = 24)
# 0.5 deg area: (24*24 * 0.25 * 5^2) = 3600 #km2

# inds = which(LCs == "Forest")
# temp = lst_mat[inds] %>% abind(along = 3)
# {
#     mat = apply_3d(temp, 3, rowSums2)
#     mat = mat[, -(1:3)] - mat[, 3] # 2003 benchmark
#     mat[mat == 0] = NA
#     df <- as.data.frame(mat) %>% set_colnames(paste0("Y", 2004:2018))
# }
# fwrite(df, "forest_annual.csv")
# {
#     grid@data <- df
#     # r = brick(grid)
#     brks = seq(0, 20, 2.5) %>% c(Inf) %>% c(-rev(.), .) %>% unique()
#     ncol = length(brks) - 1
#     cols = rcolors::get_color("MPL_RdYlGn", ncol)
#     p <- spplot(grid[,],
#                 at = brks,
#                 col.regions = cols,
#                 as.table = TRUE)
#     write_fig(p, "a.png", 10, 10)
# }
# grid2@data <- l$trend %>% as.data.frame()

# df[, area := slp*3600] # km^2
# df$LC %<>% factor(IGBP006_names)
# xlim = range[1:2]
# ylim = range[3:4]
