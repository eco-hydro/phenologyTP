## post-process
source('test/main_pkgs.R')

# MAIN SCRIPTS ------------------------------------------------------------
# tidy meteorological forcing
# multiple annual meteorological forcing
load("data/00basement_TP.rda")
load(file_pheno_010)
range <- c(73, 105, 25, 40)

indir <- "G:/SciData/China/Data_forcing_01dy_010deg"
indir <- "N:/DATA/3h metrology data/Data_forcing_01dy_010deg"
files <- dir(indir, "*.nc$", full.names = T)
# varnames <- basename(files) %>% str_extract(".{4}(?=_)")
files_lst <- split(files, substr(basename(files), 1, 4))

## 1. Prepare daily forcing, (prec, srad, temp), all data of Tibet-Plateau
foreach(files = files_lst, var = names(files_lst)) %do% {
    # test aggregate array
    print(var)
    eval(parse(text = glue("lst_{var} <- read_forcing(files, I_grid_10)")))
    eval(parse(text = glue("save(lst_{var}, file = 'INPUT/TP_010deg_{var}.rda')")))    
}

## 2. 3hourly temperature to Tmin, Tmax, Tavg ----------------------------------
temp3h_ToDaily = TRUE
if (temp3h_ToDaily) {
    source("test/main_pkgs.R")
    load("data/00basement_TP.rda")
    load(file_pheno_010)
    range <- c(73, 105, 25, 40)
    
    # indir <- "F:/ChinaForcing/Temp3h/"
    indir <- "N:/DATA/3h metrology data/3h_temp"
    files <- dir(indir, "*.nc$", full.names = T)
    # file  <- files[1]
    
    # has resampled to n
    Id_grid <- I_grid_10
    # Id_grid <- I_grid2_10 # ID num of rectangle grid
    lst_temp <- foreach(file = files, i = icount()) %do% {
        runningId(i, 1, length(files))
        r <- ncread_cmip5(file, "temp", range = range, ntime = -1, 
                          convertTo2d = FALSE, 
                          adjust_lon = FALSE, delta = 0, offset = -273.15)
        # mat_Tavg <- array_3dTo2d(r$value, Id_grid) %>% array(dim = c(nrow(.), 8, ncol(.)/8)) %>% apply_3d(dim = 2)
        mat_Tmax <- array_3dTo2d(flipud(r$data), Id_grid) %>% array(dim = c(nrow(.), 8, ncol(.)/8)) %>% apply_3d(dim = 2, FUN = rowMaxs)
        mat_Tmin <- array_3dTo2d(flipud(r$data), Id_grid) %>% array(dim = c(nrow(.), 8, ncol(.)/8)) %>% apply_3d(dim = 2, FUN = rowMins)
        
        list(Tmax = mat_Tmax, Tmin = mat_Tmin) # Tavg = mat_Tavg, 
    }
    
    dates = seq(ymd("1981-01-01"), ymd("2018-12-31"), "day")
    lst_temp2 <- lst_temp %>% purrr::transpose() %>% map(~do.call(cbind, .))
    
    save(lst_temp2, dates, file = "INPUT/TP_010deg_temp2_full.rda")
    # fwrite(lst_temp$Tavg, "TP_010deg_Tavg.csv")
}

# used for Fig1 ----------------------------------------------------------------
mutiAnnual <- function(lst){
    map(lst, rowMeans2, na.rm = T) %>% do.call(cbind, .) %>% rowMeans2(na.rm = T)
}

# save(dem, multiAnnual_prec, multiAnnual_srad, multiAnnual_Tavg, )
Fig1 <- TRUE
if (Fig1) {
    source("test/main_pkgs.R")
    load("data/00basement_TP.rda")
    
    load("INPUT/TP_010deg_temp2.rda")
    dates = seq(ymd("1981-01-01"), ymd("2018-12-31"), "day")
    
    multiAnnual_Tmax = apply_3d(lst_temp2$Tmax, 2, by = year(dates)) %>% rowMeans2()
    multiAnnual_Tmin = apply_3d(lst_temp2$Tmin, 2, by = year(dates)) %>% rowMeans2()
    
    load("INPUT/TP_010deg_prec.rda")
    load("INPUT/TP_010deg_temp.rda")
    load("INPUT/TP_010deg_srad.rda")
    
    # dem <- rgdal::readGDAL("D:/Documents/ArcGIS/phenology_TP/grid/tp_dem_005deg") %>% set_names("dem")
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
    
    # p_dem <- spplot_grid(dem, "dem" , 
    #             panel.title = "(a) dem (m)", pars = pars, 
    #             brks = brks$dem, colors = colors$dem, col.rev = TRUE,
    #             ylab.offset = ylab.offset)
    
    ## 2. Tavg
    d <- data.table(Tavg = multiAnnual_Tavg, Tmax = multiAnnual_Tmax, Tmin = multiAnnual_Tmin, 
                    Srad = multiAnnual_srad, Prec = multiAnnual_prec*365)
    gridclip_10@data <- data.frame(d)
    
    titles_EN <- c("(a) Tavg (℃)", "(b) Tmax (℃)", "(c) Tmin (℃)", 
        expression(bold('(c) Prec (mm '* a^-1*")")), 
        expression(bold('(d) Srad (W '*m^-2*d^-1 * ")")))
    titles_ZH <- c(
        expression(bold("(a) 平均温度 (℃)")),
        expression(bold("(b) 最高温度 (℃)")), 
        expression(bold("(c) 最低温度 (℃)")), 
        expression(bold('(d) 年降水 (mm '* a^-1*")")), 
        expression(bold('(e) 入射短波辐射 (W '*m^-2*d^-1 * ")")))
    {
        brks   <- .brks[c("Tavg", "Tavg", "Tavg", "Prec", "Srad")]
        colors <- .colors[c("Tavg", "Tavg", "Tavg", "Prec", "Srad")]
        varnames <- c("Tavg", "Tmax", "Tmin", "Prec", "Srad")
        ps <- foreach(i = 1:5, color = colors, brk = brks, var = varnames, icount()) %do% {
            p <- spplot_grid(gridclip_10, var, 
                              panel.title = titles_ZH[i], 
                              pars = pars, 
                              brks = brk, 
                              ylim = c(25.99, 40.2),
                              colors = color,
                              ylab.offset = ylab.offset, 
                              sp.layout = sp_layout)
            p + theme_lattice(plot.margin = c(0.2, 2.5, -1, 0.2))
        }
        g <- arrangeGrob(grobs = ps, nrow = 3)
        write_fig(g, "Figure_2-1_mete_multi-annual_dist.tif", 9.8, 7)    
    }
    
    # grid.newpage(); grid.draw(g)
}

par(family = "song")
