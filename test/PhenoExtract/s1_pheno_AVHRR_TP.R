library(phenologyy)
library(lubridate)
source('test/main_TSF.R')
load("data/00basement_TP.rda")
I_grid <- fread("data/Id_avhrr_TP.txt")$V1

# MAIN SCRIPTS ------------------------------------------------------------

files <- dir("G:/Github/data", "*.nc", full.names = T)
fid   <- nc_open(files[1])

# I_grid <- fid$dim$Id$vals
# fwrite(data.table(I_grid), "Id_avhrr_TP.txt", col.names = F)
mat_y  <- ncvar_get(fid, "NDVI")
mat_QC <- ncvar_get(fid, "VI_quality")

# snow info according to Tavg
file_snow <- "G:/Github/data/SnowDays_TP_1981_2015.mat"
snowdays  <- readMat(file_snow)$snowdays[,, 25:840]
dims      <- dim(snowdays)
mat_snow  <- array(as.numeric(snowdays), dim = c(prod(dims[1:2]), dims[3]))[I_grid, ]
# continous Tavg <= 0, as potential snow contaminated.
mat_QC[mat_snow >= 5] <- 2

file_qc <- "G:/Github/phenology/phenology/phenology_TP/TSF/TP_NDVI_AVHRR_qc_nosnow.txt"
write_input(mat_QC, file_qc, nptperyear = 24) 

mat_y <- fread("G:/Github/phenology/China_Phenology/MATLAB/TSM/TSM_debug/TP_NDVI_AVHRR_nosnow.txt", 
               skip = 1)[, -(1:24)] %>% as.matrix()

# d %>% mutate(year = year(t), month = month(t))
# d[, `:=`(month = month(t), )]
# d_date[, Ingrow := month %in% c(11:12, 1:3)]

{
    library(lattice)
    points <- rgdal::readOGR("D:/Documents/ArcGIS/phenology_TP/shp/sp_code.shp")
    proj4string(points) <- proj4string(gridclip)
    I_test <- over(points, gridclip)$id %>% rm_empty()
    
    spplot(gridclip, ap.layout = list("sp.points", points))
    
    outfile <- "../phenofit/inst/shiny/phenofit/data/TP_test100_nosnow.csv"
    d_date   <- get_date_AVHRR()
    df_input <- phenofit_input.avhrr(mat_y, mat_qc, d_date, wmax = 0.8, I_test, outfile = outfile)
}

plot(gridclip)
d_date   <- get_date_AVHRR()
df_input <- phenofit_input.avhrr(mat_y, mat_qc, d_date, wmax = 0.8, I_st, st, outfile = "TP_rep26.csv")
fwrite(st, file = "TP_rep26_st.csv")

# df_input <- phenofit_input.avhrr(mat_y, mat_qc, d_date, wmax = 0.8, outfile = "phenofit_input_TP_avhrr.csv")


# write_input(r$y, file = "TP_rep26_y.txt", 24)
# write_input(r$qc, file = "TP_rep26_w.txt", 24)
fwrite(st, file = "TP_rep26_st.csv")
# write_input(r$qc, file = "TP_rep29_w.txt", 24)
## 1. check multiple annual distribution of NDVI, QC, and snow

r_y    <- multiYear_mean(mat_y)
r_QC   <- multiYear_mean(mat_QC)
r_snow <- multiYear_mean(mat_snow)

# Fitting method (1/2/3): (SG/AG/DL)
meths <- c("SG", "AG", "DL")

source('TSF_main.R')

foreach(FUN = 2:3, meth = meths[2:3]) %do% {
    job_name <- sprintf("NDVI3g.v1_%s", meth)
    print(job_name)
    TSF_main(mat_y, mat_QC, job_name = job_name, 
             nptperyear = 24, nyear = 34, 
             FUN = FUN, iters = 3, half_win = 7, cache = TRUE, overwrite = FALSE)
}
    
{
    plot_annual <- function(data, main, file){
        gridclip@data <- as.data.frame(data) 
        zcol <- colnames(gridclip@data)
        
        p <- spplot(gridclip, main = main, 
               as.table = T, strip = FALSE, 
               # at = c(0, 2, 5, 7, 10, 16), 
               panel = function (x, y, z, subscripts, ...,  sp.layout) 
               {
                   sppanel(list(sp.layout), panel.number(), first = TRUE)
                   panel.levelplot.raster(x, y, z, subscripts, ..., interpolate = T)
                   # panel.contourplot(x, y, z, subscripts, ..., contour = TRUE, labels = F)
                   sppanel(list(sp.layout), panel.number(), first = FALSE)
                   
                   i <- panel.number()
                   panel.text(76, 38.4, paste0("(",letters[i], ") ", zcol[i]), #english name: New_names[i])
                              # fontfamily = "Times", 
                              cex = 1.1, font = 1, adj = 0)
                   # panel.text(76.6, 40.8 +heights[i], metricsEN[i], fontfamily = "Times", cex = 1.2, font = 2, adj = 0)
                   # panel.addbarchart(z, subscripts, cols, showCluster = F, A = A, by = 0.9, ntick = ntick, ...)
               })
        
        width = 900; height = 520; ratio = 3.5
        # if (saveFIG)
        #     CairoPNG(filename = file, width*ratio, height*ratio, dpi = 250, pointsize = 12)   
        
        print(p)
    }
    plot_annual(r_y, "potential snow days")
}

plot_annual(r_y   , "NDVI")
plot_annual(r_QC  , "qc flag")


# system.time(r <- array_mean(snow2, 3))
# gridclip@data <- as.data.frame(r_y)
# spplot(gridclip, 1:24, as.table = T)
