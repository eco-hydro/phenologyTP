source("test/main_pkgs.R")

# grouped into: Forest, Shrubland, Grassland, Cropland (CNV and CRO), Urban and Water and others
files <- dir("INPUT/tif/", "landcover_perc_G010_", full.names = TRUE) %>%
    set_year_names()

years <- 2003:2017
# file = files[1]
## x = readGDAL(file, silent = TRUE)
lst = llply(files, readGDAL, silent = FALSE, .progress = "text")
lst_mat <- map(IGBP006_codes, function(i){
    ans <- map(lst, ~.x@data[[i]]) %>% do.call(cbind, .) %>% set_colnames(years)
})

write_fig({
    spplot(x, 2:4)
}, "a.png", 10, 6)
# 2008-2018 and 2003-2007
mat <- lst_mat$ENF
lc_diff <- function(mat) {
    years1 = 2003:2007
    years2 = 2008:2017
    vec.a <- rowMeans2(mat, cols = years1 - 2002, na.rm = TRUE)
    vec.b <- rowMeans2(mat, cols = years2 - 2002, na.rm = TRUE)

    vec.b - vec.a
}
lst_diff <- map(lst_mat, lc_diff)
df_diff <- do.call(cbind, lst_diff) %>% data.table()
fwrite(df_diff, "INPUT/lc_diff (2008-2017)-(2003-2007).csv")

# check2018
# lc_diff_2018 <- function(mat) {
#     years1 = 2008:2017
#     years2 = 2018
#     vec.a <- rowMeans2(mat, cols = years1 - 2000, na.rm = TRUE)
#     vec.b <- rowMeans2(mat, cols = years2 - 2000, na.rm = TRUE)
# 
#     vec.b - vec.a
# }
lst_diff <- map(lst_mat, lc_diff_2018)
df_diff <- do.call(cbind, lst_diff) %>% data.table()
fwrite(df_diff, "INPUT/lc_diff (2018)-(2008-2017).csv")


# 计算mktrend
r = lst$`2001`
grid = r[1]
# mat = r$band1 %>% array(r@grid@cells.dim)
mat = lst_mat$ENF
# write_fig(expression({
#     # spplot(x, 1)
#     image(mat %>% flipud())
# }), "a.png", 10, 6)

# library(Ipaper)
# InitCluster(12)
# lc_trends = foreach(mat = lst_mat[I_lc],
#                     prefix = IGBP006_names[I_lc], i = icount()) %dopar% {
#      temp = tryCatch({
#             get_trend(mat, limits = NULL, prefix = prefix)
#         }, error = function(e) {
#             message(sprintf("[e] i = %5d, %s", i, e$message))
#             #  c(NA_real_, NA_real_)
#         }
#      )
# }

# {
#     l <- lc_trends$URB
#     # cols <- rcolors$GMT_red2green %T>% show_col()
#     {

#         write_fig(expression({
#             print({
#                 spplot(s2, at = brks, col.regions = cols,
#                        sp.layout = sp_layout)
#             })
#         }), "Urban.png", 10, 6)
#     }
# }
