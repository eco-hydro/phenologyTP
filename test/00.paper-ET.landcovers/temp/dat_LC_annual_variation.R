source("test/main_pkgs.R")

## grid template ---------------------------------------------------------------
range <- c(-180, 180, -60, 90)
grid2 <- get_grid(range, cellsize = 1/10, type = "vec")
area <- raster::area(raster(grid2)) %>% values()
grid2$area <- area # unit km^2
# write_fig(expression({
#     print(spplot(grid2, 2))
# }), "a.tif")

## annual variation ------------------------------------------------------------
indir = "INPUT/tif/"
files_lc <- dir(indir, "MCD12Q1_", full.names = TRUE) %>%
    set_year_names()
file <- files_lc[1]

lst <- foreach(file = files_lc, i = icount()) %do% {
    grid <- readGDAL(file)
    d <- aggregate_major(grid)
    temp <- cbind(I = 1:nrow(d), area, d)
}

info <- foreach(temp = lst, i = icount()) %do% {
    x = melt(temp, c("I", "area"))
    x[, .(area = sum(area*value, na.rm = TRUE)/100/1e6), .(variable)]
}
tbl <- melt_list(info, "year") %>% dcast2("variable", "area")
write_list2xlsx(list(tbl = tbl), "LC_annual_area.xlsx")
file.show("LC_annual_area.xlsx")
