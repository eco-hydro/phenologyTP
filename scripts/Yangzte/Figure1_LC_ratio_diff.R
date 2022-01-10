# load("INPUT/lc_trends.rda")
# visualization
# grid2 <- as(grid, "SpatialPixelsDataFrame")
source("test/main_pkgs.R")
grid5 <- get_grid(range_global, cellsize = 0.1, type = "vec")
r = over(grid5, shp)
inds = r$Id %>% which.notna()

library(sf)
d_diff       <- fread("INPUT/lc_diff (2008-2017)-(2003-2007).csv")
# d_diff       <- fread("INPUT/lc_diff (2008-2018)-(2003-2007).csv")
# d_diff       <- fread("INPUT/lc_diff (2018)-(2008-2017).csv")
{
    d_diff_major <- aggregate_lc(d_diff %>% as.matrix)[, ]
    # df = resample_grid(grid, d_diff, fact = 5)@data %>% add_column_id() %>% melt('I', variable.name = "LC")
    df_major = resample_grid(grid_global, d_diff_major, fact = 1)@data %>% 
        data.table() %>% 
        .[inds, ] %>% melt_lc()
    df_major$LC %<>% factor(LCs_types)
    df_major[value == 0, value := NA_real_]
}
# d_bg = df_major[, .(value = sum(value, na.rm = TRUE)), .(I)]
# d_bg[value == 0, value := NA_real_]
# grid2 <- grid5
# grid2@data <- d_bg

r = grid5[inds, ]
r@data <- df[, -1]
writeRaster(brick(r), "yangtze_LC_changeRatio.tif")
#%>% brick()
# df_major$I %>% unique()
{
    sp_region <- list("sp.polygons", shp, lwd = 1, 
                      first = F)
    # load_all("../lattice.layers.R")
    lattice.layers::set_options(list(style = "CH", family_CH = "rTimes"))
    max = 4
    # brks <-  c(-Inf, seq(-2, 2, 0.2), Inf) # perc
    # brks <-  c(-Inf, seq(-max, max, 0.4), Inf) # km^2, 3600 in total
    brks <- c(0.5, 1, 2, 5) %>% c(-Inf, -rev(.), 0, ., Inf) # 
    ncol <- length(brks) - 1
    cols <- colorRampPalette(c("red", "white", "green"))(ncol)
    # cols <- get_color("GMT_red2green", ncol*2) %>% rev()
    # cols <- get_color("BlGrYeOrReVi200", ncol) %>% rev()

    stat = list(show = TRUE, name = "RC", loc = c(107, 34.8), 
                digit = 2, include.sd = TRUE, 
                FUN = weightedMean)
    pars = list(title = list(x = 98, y = 35+0.5, cex = 1.5))
    pars$panel.backgroundcol = 'gray'
    
    # p <- levelplot2(value ~ s1+s2 | LC,
    #                 # df,
    #                 df_major,
    p <- lattice.layers::sp_plot(
                    # df[!(LC %in% c("UNC", "water"))], # blank
                    # df[LC %in% IGBP006_names[1:4]],
                    grid5[inds, ], 
                    formula = value ~ lon+lat | LC, 
                    df = df_major,
                    panel.titles_full = label_tag(LCs_types_zh),
                    # df.mask = df[, .(LC, mask = pval <= 0.05)],
                    colors = cols, brks = brks,
                    layout = c(2, 4),
                    pars = pars,
                    par.strip.text = list(cex = 1.5, font = 2, fontfamily = "rTimes", lineheight = 2),
                    ylim = c(24, 36) + c(-1, 1)*0.5,
                    xlim = c(90, 122),
                    aspect = 0.55,
                    unit = "(%)", unit.adj = 0.4,
                    legend.num2factor = TRUE,
                    # sp.layout = list(bar, l1, l2),
                    sp.layout = list(sp_region, sp_river_L2, sp_river_L1),
                    colorkey = list(space = "right", height  = 0.95),
                    interpolate = FALSE,
                    stat = stat
    ) +
        theme_lattice(key.margin  = c(1, 1.5, 0, 0),
                      plot.margin = c(0, 4, -1, 1))
    write_fig(p, "Figure1_Yangtze_LC_changes_major_(2008-2017)-(2003-2007)_V2.png", 10, 9.8, show = TRUE)
}

d <- na.omit(df_major)
d %<>% plyr::mutate(lev = cut(value, brks))
tbl1_count <- d[, as.list(table(lev)), .(LC)]
tbl1_perc  <- d[, as.list(table(lev) %>% {./sum(.)*100}), .(LC)]

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
