library(Ipaper)
library(Rcmip5)
library(phenofit)
library(sp)
library(magrittr)
library(phenology)
library(lattice)
library(foreach)
library(iterators)
library(tidyverse)
library(lubridate)
library(oce)

read_forcing <- function(files, I_grid){
    years <- basename(files) %>% str_extract("(?<=_)\\d{4}") %>% as.numeric()
    
    varname <- basename(files[1]) %>% str_extract(".{4}(?=_)") 
    offset = 0; scale = 1
    
    read_file <- function(file, I_grid){
        if (varname == "temp") {
            offset = -273.15
        } else if (varname == "prec") {
            scale = 24
        } 
        
        r <- ncread_cmip5(file, range = range, ntime = -1, 
                          shiftLon = FALSE, delta = 0, 
                          offset = offset, scale = scale)
        mat <- r$value
        if (!missing(I_grid)) {
            mat <- array_3dTo2d(mat, I_grid)
        }
        mat
    }
    llply(files, read_file, I_grid = I_grid, .progress = "text") %>% set_names(years)
}

# MAIN SCRIPTS ------------------------------------------------------------
# tidy meteorological forcing
# multiple annual meteorological forcing
load("data/00basement_TP.rda")

indir <- "G:/SciData/中国数据/Data_forcing_01dy_010deg"
files <- dir(indir, "*.nc$", full.names = T)
# varnames <- basename(files) %>% str_extract(".{4}(?=_)")
files_lst <- split(files, substr(basename(files), 1, 4))

foreach(files = files_lst, varname = names(files_lst)) %do% {
    # test aggregate array
    print(varname)
    eval(parse(text = sprintf("lst_%s <- read_forcing(files, I_grid_10)", varname)))
    eval(parse(text = sprintf("save(lst_%s, file = 'TP_010deg_%s')", varname, varname)))    
}


## 2. 3hourly temperature to Tmin, Tmax, Tavg ----------------------------------
temp3h_ToDaily = TRUE
if (temp3h_ToDaily) {
    source("test/main_pkgs.R")
    load("data/00basement_TP.rda")
    load(file_pheno_010)
    
    files <- dir("F:/ChinaForcing/Temp3h/", "*.nc$", full.names = T)
    file  <- files[1]
    
    # has resampled to n
    Id_grid <- I_grid_10
    # Id_grid <- I_grid2_10 # ID num of rectangle grid
    
    lst_temp <- foreach(file = files, i = icount()) %do% {
        runningId(i, 1, length(files))
        r <- ncread_cmip5(file, "temp", range = range, ntime = -1, 
                          shiftLon = FALSE, delta = 0, offset = -273.15)
        # mat_Tavg <- array_3dTo2d(r$value, Id_grid) %>% array(dim = c(nrow(.), 8, ncol(.)/8)) %>% apply_3d(dim = 2)
        mat_Tmax <- array_3dTo2d(r$value, Id_grid) %>% array(dim = c(nrow(.), 8, ncol(.)/8)) %>% apply_3d(dim = 2, FUN = rowMaxs)
        mat_Tmin <- array_3dTo2d(r$value, Id_grid) %>% array(dim = c(nrow(.), 8, ncol(.)/8)) %>% apply_3d(dim = 2, FUN = rowMins)
        
        list(Tmax = mat_Tmax, Tmin = mat_Tmin) # Tavg = mat_Tavg, 
    } 
    # lst_temp <- lst_temp %>% purrr::transpose()
    lst_temp <- lst_temp %>% purrr::transpose() %>% map(~do.call(cbind, .))
    save(lst_temp, file = "TP_010deg_temp3.rda")
    # fwrite(lst_temp$Tavg, "TP_010deg_Tavg.csv")
}


# used for Fig1
mutiAnnual <- function(lst){
    map(lst, rowMeans2, na.rm = T) %>% do.call(cbind, .) %>% rowMeans2(na.rm = T)
}
# save(dem, multiAnnual_prec, multiAnnual_srad, multiAnnual_Tavg, )

Fig1 <- TRUE
if (Fig1) {
    source("test/main_pkgs.R")
    load("data/00basement_TP.rda")
    
    dem <- rgdal::readGDAL("D:/Documents/ArcGIS/phenology_TP/grid/tp_dem_005deg") %>% set_names("dem")
    multiAnnual_prec <- lst_prec %>% mutiAnnual
    multiAnnual_srad <- lst_srad %>% mutiAnnual
    multiAnnual_Tavg <- lst_temp %>% mutiAnnual

    pars = list(title = list(x=77, y=39, cex=1.5), 
                hist = list(origin.x=77, origin.y=28, A=15, by = 0.4))
    ylab.offset <- 3
    # col_fun.sos <- c("green4", "grey80", "red") %>% rev() %>% colorRampPalette()
    # colors <- c("red", "grey80", "blue4") %>% rep() #RColorBrewer::brewer.pal(9, "RdBu") # %>% rev()
    # colors <- c("red", "grey80", "green4") # , "yellow"
    # col_fun.eos <- colorRampPalette(colors)
    
    p_dem <- spplot_grid(dem, "dem" , 
                panel.title = "(a) dem (m)", pars = pars, 
                brks = brks$dem, colors = colors$dem, col.rev = TRUE,
                ylab.offset = ylab.offset)
    
    ## 2. Tavg
    gridclip_10@data <- data.frame(Tavg = multiAnnual_Tavg)
    p_Tavg <- spplot_grid(gridclip_10, "Tavg", 
                panel.title = "(b) Tavg (℃)", pars = pars, 
                brks = brks$Tavg, 
                colors = colors$Tavg,
                ylab.offset = ylab.offset)
    ## 3. Prec
    gridclip_10@data <- data.frame(Prec = multiAnnual_prec*365)
    p_prec <- spplot_grid(gridclip_10, "Prec", 
                          panel.title = expression(bold('(c) Prec (mm '* a^-1*")")), 
                          brks = brks$Prec, pars = pars, 
                          colors = colors$Prec, 
                          ylab.offset = ylab.offset)
    
    ## 3. Prec
    gridclip_10@data <- data.frame(Srad = multiAnnual_srad)
    p_srad <- spplot_grid(gridclip_10, "Srad", 
                          panel.title = expression(bold('(d) Srad (W '*m^-2*d^-1 * ")")), 
                          brks = brks$Srad, pars = pars, 
                          colors = colors$Srad, 
                          ylab.offset = ylab.offset)
    p_srad
    g <- arrangeGrob(grobs = list(p_dem, p_Tavg, p_prec, p_srad), nrow = 2)
    write_fig(g, "Figure_s1_mete_multi-annual_dist.pdf", 11, 5.2)
    # grid.newpage(); grid.draw(g)
}

