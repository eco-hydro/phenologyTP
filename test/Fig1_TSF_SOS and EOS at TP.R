# read and plot TSF phenology
# source("test/00_shp_basement_TP.R")


## MAIN SCIPTS -----------------------------------------------------------------

load_data = nrun == 0#FALSE
if (load_data) {
    source("test/00_shp_basement_TP.R")
    
    files <- dir("TSF", "*.tpa", full.names = T) %>% 
    set_names(basename(.) %>% str_extract("(?<=_).{2}(?=_)"))
    date_AVHRR <- get_date_AVHRR()

    lst_tpa <- llply(files, function(file){
        d_tpa <- read_tpa(file) %>% data.table()
        time2date_tpa(d_tpa, date_AVHRR)
        
        d <- d_tpa[, lapply(.SD, yday), .SDcols = c("date_start", "date_end", "date_peak")] %>% 
            cbind(d_tpa[, 1:3, ], .)
    }, .progress = "text")
    
    # multiple annual average 
    lst_avg <- lst_tpa %>% map(function(d){
        d[, lapply(.SD, mean, na.rm=F), .(row), .SDcols = c("date_start", "date_end", "date_peak")]
    })
}

# parameters of figure 
{
    source("test/TSF_main.R")
    source("R/panel.barchart.sp.R")
    
    sp_layout <- list("sp.polygons", p_TP, first = FALSE)
    
    brks_SOS  <- c(-Inf, seq(110, 150, 5/2), Inf)
    # brks_SOS  <- c(-Inf, seq(100, 150, 10), Inf)
    brks_EOS <- c(-Inf, seq(255, 285, 5/2), Inf)
    A = 20
    ntick = 2

    col_fun.sos <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlGn"))
    colors <- RColorBrewer::brewer.pal(9, "RdBu") # %>% rev()
    colors <- c("red", "grey80", "green4") # , "yellow"
    col_fun.eos <- colorRampPalette(colors)
}

{
    ps <- foreach(d_avg = lst_avg, meth = names(lst_avg)) %do% {
        gridclip <- fill_grid(gridclip, d_avg)

        p_SOS <- spplot_single(gridclip, "date_start", 
            panel.title = sprintf("%s (a) SOS", meth), 
            brks_SOS, colfun = col_fun.sos, col.rev = TRUE)
        p_EOS <- spplot_single(gridclip, "date_end", 
            panel.title = sprintf("%s (b) EOS", meth), 
            brks = brks_EOS, colfun = col_fun.eos, col.rev = FALSE, 
            by = 0.6)
        arrangeGrob(p_SOS, p_EOS, nrow = 1)
    }
    g <- arrangeGrob(grobs = ps, padding = unit(0, "line"))

    # p_EOS
    cairo_pdf("a.pdf", 11, 5.2)
    grid.draw(g)
    dev.off()
    # brks <- brks_SOS
    # spplot(gridclip, 1, at = brks, drop.unused.levels = FALSE,
    #        colorkey=list(space = 'right',
    #                      # at = check_brks(brks),
    #                      tri.upper = 0.1, tri.lower = 0.1, 
    #                      tck = -4,
    #                      # labels=list(at = check_brks(brks), labels = brks,
    #                      #             cex=1.2, fontface='bold', fontfamily='times'),
    #                      height = 1)
    # )
}


# trellis.par.set(opt_trellis)
# 
# init_lattice <- function(){
#     # trellis.par.set(theme = col.whitebg())
#     delta_x <- 0
#     delta_y <- 0
#     
#     l_x <- list(x = delta_x, units = "inches")
#     l_y <- list(x = delta_y, units = "inches") 
#     opt_lattice <- list(
#         layout.widths = list(
#             left.padding = l_x,
#             right.padding = l_x
#             # key.right = list(x = 0.2, units = "inches")
#         ),
#         layout.heights = list(
#             bottom.padding = list(x = 0.2, units = "cm") , 
#             top.padding = l_y,
#             key.axis.padding = list(x = 0, units = "inches")
#             # axis.xlab.padding = list(x = -0.1, units = "inches")
#         )
#     )
#     lattice.options(opt_lattice)
# }
# init_lattice()
