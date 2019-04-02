opt_trellis <- list(
    layout.heights = list(
        top.padding       = 0,
        main.key.padding  = 0,
        key.axis.padding  = 0,
        axis.xlab.padding = 0,
        xlab.key.padding  = 0,
        key.sub.padding   = 0,
        bottom.padding    = 0
    ), 
    layout.widths = list(
        left.padding      = 1, # left
        right.padding     = 1, # right
        key.ylab.padding  = 1, # axis.y padding
        ylab.axis.padding = 0, # same as above
        axis.key.padding  = 0  # legend left padding
    ),
    axis.line = list(col = "white")
)

check_brks <- function(brks){
    nbrk  <- length(brks)
    delta <- median(diff(brks))
    if (is.infinite(brks[1])) {
        brks[1] <- brks[2] - delta
    }
    if (is.infinite(brks[nbrk])) {
        brks[nbrk] <- brks[nbrk - 1] + delta
    }
    brks
}

panel_hist <- function(x, y, z, subscripts, ...,  
    grob = NULL, bbox, sub.hist = TRUE, sp.layout, 
    pars
){
    dot <- list(...)

    # print(str(listk(x, y, z, subscripts, ...)))#debug code
    sppanel(list(sp.layout), panel.number(), first = TRUE)
    panel.levelplot.raster(x, y, z, subscripts, ..., interpolate = TRUE)

    if (!is.null(dot$contour) && dot$contour) {
        # dot$at <- c(5000)
        dot$region <- FALSE
        params <- listk(x, y, z, subscripts, contour = TRUE, interpolate = TRUE, 
                        lwd = 0.1, lty =2,
                        labels = FALSE, label.style = "flat") %>% c(dot)
        do.call(panel.contourplot, params)
    }

    # subplot
    if (!is.null(grob)) { panel.annotation(grob, bbox) }

    sppanel(list(sp.layout), panel.number(), first = FALSE)
    
    i <- ifelse(is.null(dot$order), panel.number(), dot$order)
    panel.title <- ifelse(is.null(dot$panel.title), 
                          paste0("(",letters[i], ") ", dot$panel.titles[i]), 
                          dot$panel.title)
    # browser()
    panel.text(pars$title$x, pars$title$y, panel.title, #english name: New_names[i])
               fontfamily = "Times", cex = pars$title$cex, font = 2, adj = 0)
    if (sub.hist) {
        params <- listk(z, subscripts, ntick = 3, ...) %>% 
            c(., pars$hist)
        # browser()
        do.call(panel.barchart2, params)
    }
}

xlim <- c(73.5049, 104.9725)
ylim <- c(25.99376, 40.12632)

#' spplot_grid
#' 
#' @importFrom sp spplot 
#' @import lattice
#' 
#' @export
spplot_grid <- function(
        grid, zcols, 
        panel.title = NULL, 
        brks, colors, col.rev = FALSE, 
        toFactor = FALSE, 
        sub.hist = TRUE, 
        grob = NULL, bbox = c(0, 0.5, 0.5, 1),
        xlim = c(73.5049, 104.9725), ylim = c(25.99376, 40.12632),
        pars = list(title = list(x=77, y=39, cex=1.5), 
            hist = list(origin.x=77, origin.y=28, A=15, by = 0.4)),
        legend.space = "right", colorkey = TRUE, ...)
{
    if (missing(zcols)) { zcols <- names(grid) }
    zcols %<>% intersect(names(grid@data))

    if (missing(colors)){ colors <- c("red", "grey80", "blue4") }
    if (missing(brks)) {
        brks <- pretty(grid@data[[1]]) 
        cols <- colors
    } else {
        colfun <- colors %>% rep %>% colorRampPalette()
        ncolor <- length(brks) - 1
        cols <- colfun(ncolor) #%>% rev()    

        # cut into factor
        
        df <- grid@data[, zcols, drop = FALSE]
        # browser()
        # 
        if (toFactor) {
            # drawkey can't support factor well
            df <- lapply(df, cut, brks) %>% as.data.frame()
        }
        levels <- cut(1, brks) %>% levels()
        grid@data <- df
    }
    if (col.rev) cols %<>% rev()   
    
    params <- list(
        grid, zcols,
        col.regions = cols,
        panel.titles = zcols, 
        panel = panel_hist, panel.title = panel.title,
        sub.hist = sub.hist, 
        brks = brks,
        xlim = xlim, ylim = ylim, ..., strip = FALSE, as.table = TRUE,
        sp.layout = sp_layout,
        drop.unused.levels = FALSE, 
        par.settings = opt_trellis, 
        grob = grob, bbox = bbox, 
        pars = pars
    )
    
    is_factor <- is.factor(grid@data[, zcols, drop = FALSE][[1]])
 
    if (!is_factor){ params$at <- brks }
    if (colorkey) {
        params$colorkey <- get_colorkey(brks, legend.space, is_factor)$colorkey
    } else {
        params$colorkey <- FALSE
    }

    do.call(spplot, params)
}
