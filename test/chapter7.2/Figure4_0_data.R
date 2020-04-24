source("test/main_pkgs.R")
# install("/mnt/e/github/hydro/rcolors")

range = c(-180, 180, -60, 90)
grid_010 <- get_grid(range, cellsize = 0.1, type = "vec")
r_010 <- raster(grid_010)
# 1. process gldas data
{
    bands <- c('PET' , 'Ec', 'Es', 'Ei', 'Prcp', 'Tair', 'Rl', 'Rs')
    gldas <- readGDAL("INPUT/GLDAS_mete_MultiAnnual_average-2003-2017.tif") # 0.25 deg
    names(gldas) <- bands

    gldas$Aridity <- gldas$PET/gldas$Prcp
    gldas$D  <- gldas$Prcp - gldas$PET
    gldas_r = brick(gldas)
    mat = as.array(gldas_r)
    # AI = mat[,,1] / mat[,,5]

    varnames = c("Aridity", "Tair", "Prcp", "D")
    grid_mete_025 <- gldas[varnames]
    gldas$Aridity %<>% Ipaper::clamp(lims = c(0, 20))

    r_mete_010 = resample(brick(grid_mete_025), r_010)
    d_mete = values(r_mete_010) %>% data.table()
    # vals = as.array(mete_010)
    # dim = dim(vals)
    # d = aperm(vals, c(2, 1, 3)) %>% array(c(prod(dim[1:2]), dim[3])) %>%
    #     as.data.frame() %>% set_colnames(varnames)
    grid_010@data <- d_mete
    # spplot(grid_010, 1, at = c(-Inf, seq(0, 10, 1), Inf))
}
# d_mete = grid_mete_025@data %>% cbind(I = 1:nrow(.), .) %>% data.table()
# levelplot2(Prcp ~ s1+s2, d, gldas)
# val_AI <- values(r2)

##
# grouped into: Forest, Shrubland, Grassland, Cropland (CNV and CRO), Urban and Water and others
indir = "INPUT/tif/v015/"
files_dynamic <- dir(indir, "PML2_yearly_dynamic", full.names = TRUE) %>%
    set_year_names()
files_static  <- dir(indir, "PML2_yearly_static", full.names = TRUE) %>%
    set_year_names()
files_lai <- dir(indir, "LAI", full.names = TRUE) %>%
    set_year_names()

files_lc <- dir("INPUT/tif/", "MCD12Q1_land_perc_010", full.names = TRUE) %>%
    set_year_names()

# file = files[1]
## x = readGDAL(file, silent = TRUE)

lst_dynamic <- llply(files_dynamic, readGDAL, band = 1:4)
lst_static  <- llply(files_static , readGDAL, band = 1:4)
lst_lai <- llply(files_lai, readGDAL)

df_lai      <- get_anorm(lst_lai)
lst_dynamic  <- get_anorm(lst_dynamic)
lst_static   <- get_anorm(lst_static)

lst    <- llply(files_lc, readGDAL, silent = FALSE, .progress = "text")
lst_lc <- llply(lst, aggregate_major, .progress = "text")
df_lc  <- get_anorm_lc(lst_lc) # [3:17]

# has been tested that 'vec' style grid is right
is_test_grid = FALSE
if (is_test_grid) {
    grid2@data <- grid@data
    spplot(grid2)
}

df <- cbind(lst_dynamic[, 1:2],
            d_mete,
            lai = df_lai$band1,
            df_lc[, -(1:2)],
            set_colnames(lst_dynamic[, -(1:2)] - lst_static[, -(1:2)], c("GPP", "Ec", "Es", "Ei")))
df_sm <- df[!is.na(lai)]

file_dat_figure4 = "INPUT/dat_Figure4_df_sm.csv"
fwrite(df_sm, file_dat_figure4)

source("test/main_pkgs.R")
df_sm <- fread(file_dat_figure4)

# grid@data <- data.frame(val_AI)
## Figure4_2
# ggplot(dplot, aes(lai, value, color = prob)) + geom_point() +
#     facet_wrap(~variable, scale = "free")

# p <- ggplot(df_sm, aes(lai, GPP)) + geom_density_2d()
# bandNames = c("GPP", "Ec", "Es", "Ei", "ET_water")
# lst_dynamic <- tidy_PML(lst_dynamic, grid)
# lst_static  <- tidy_PML(lst_static, grid)
