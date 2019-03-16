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
            mat <- load_array(mat, I_grid)
        }
        mat
    }
    llply(files, read_file, I_grid = I_grid, .progress = "text") %>% set_names(years)
}

# MAIN SCRIPTS ------------------------------------------------------------
# tidy meteorological forcing
foreach(files = files_lst, varname = names(files_lst)) %do% {
    # test aggregate array
    print(varname)
    eval(parse(text = sprintf("lst_%s <- read_forcing(files, I_grid)", varname)))
    eval(parse(text = sprintf("save(lst_%s, file = 'TP_010deg_%s')", varname, varname)))    
}

mutiAnnual <- function(lst){
    map(lst, rowMeans2, na.rm = T) %>% do.call(cbind, .) %>% rowMeans2(na.rm = T)
}

save(dem, multiAnnual_prec, multiAnnual_srad, multiAnnual_Tavg, )


dem <- rgdal::readGDAL("D:/Documents/ArcGIS/phenology_TP/grid/tp_dem_005deg/")
multiAnnual_prec <- lst_prec %>% mutiAnnual
multiAnnual_srad <- lst_srad %>% mutiAnnual
multiAnnual_Tavg <- lst_temp %>% mutiAnnual


Fig1 <- TRUE
if (Fig1) {
    source("test/main_spplot.R")
    source("test/main_pkgs.R")
    col_fun.sos <- c("green4", "grey80", "red") %>% rev() %>% colorRampPalette()
    colors <- c("red", "grey80", "blue4") %>% rep() #RColorBrewer::brewer.pal(9, "RdBu") # %>% rev()
    colors <- c("red", "grey80", "green4") # , "yellow"
    col_fun.eos <- colorRampPalette(colors)
    
    p_dem <- spplot_grid(dem, "dem" , 
                panel.title = "(a) dem (m)", 
                brks = brks$dem, colors = colors$dem, col.rev = TRUE)
    
    ## 2. Tavg
    gridclip@data <- data.frame(Tavg = multiAnnual_Tavg)
    p_Tavg <- spplot_grid(gridclip, "Tavg", 
                panel.title = "(b) Tavg (℃)",
                brks = brks$Tavg, 
                colors = colors$Tavg)
    ## 3. Prec
    gridclip@data <- data.frame(Prec = multiAnnual_prec*365)
    p_prec <- spplot_grid(gridclip, "Prec", 
                          panel.title = expression(bold('(c) Prec (mm '* a^-1*")")), 
                          brks = brks$Prec, 
                          colors = colors$Prec)
    
    ## 3. Prec
    gridclip@data <- data.frame(Srad = multiAnnual_srad)
    p_srad <- spplot_grid(gridclip, "Srad", 
                          panel.title = expression(bold('(d) Srad (W '*m^-2*d^-1 * ")")), 
                          brks = brks$Srad, 
                          colors = colors$Srad)
    p_srad
    g <- arrangeGrob(grobs = list(p_dem, p_Tavg, p_prec, p_srad), nrow = 2)
    write_fig(g, "Fig1_multi_annual.pdf", 11, 5.2)
    grid.newpage(); grid.draw(g)
}