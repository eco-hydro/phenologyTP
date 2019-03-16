colors <- list(
    default = c("firebrick1","orange3", "darkgoldenrod2", "grey90", 
                             RColorBrewer::brewer.pal(9, "YlGnBu")[c(4, 6, 7)]), 
    pvalue = c("firebrick1", "orange3", "grey90", "blue", "green4") %>% rev(),
    corr = RColorBrewer::brewer.pal(10, "RdYlBu")[-c(4, 7)],
    dem  = oce::colormap(name="gmt_globe")$col0[-(1:21)],
    Tavg = RColorBrewer::brewer.pal(11, "RdYlBu") , #colormap(colormaps$temperature, 20), 
    Prec = colormap::colormap("jet", 20), 
    Srad = RColorBrewer::brewer.pal(11, "RdBu"), 
    SOS = c("green4", "grey80", "firebrick1"), 
    # EOS = c("firebrick1", "orange3", "darkgoldenrod2", "grey80", RColorBrewer::brewer.pal(9, "Greens")[c(2, 4)+2]) # "yellow4"
    # EOS = RColorBrewer::brewer.pal(11, "BrBG")[1:6]
    EOS = c("green4", "grey80", "firebrick1") %>% rev()
    # EOS = c("firebrick1","orange3", "darkgoldenrod2", "grey80", "green2")
    # EOS = brewer.pal(11, 'BrBG')
)

a <- 4500; b <- 5500 # dem
brks <- list(
    dem  = c(seq(3000, a-1, 250), seq(a, b-1, 125), seq(b, 7000, 250)),
    Tavg = c(seq(-10, 5, 1)), 
    Prec = c(seq(100, 400, 50),500, 600, 700, 800), 
    Srad = c(seq(150, 270, 10)) 
) %>% map(~c(-Inf, ., Inf))
