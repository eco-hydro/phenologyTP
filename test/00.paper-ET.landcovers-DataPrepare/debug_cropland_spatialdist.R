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
