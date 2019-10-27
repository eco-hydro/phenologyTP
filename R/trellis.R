# for Attributable change in spatial (levelplot)
# /test/s2_ReCal_Attributable_Change.R
# 
opt_trellis_strip <- list(
        layout.heights = list(
            top.padding       = 0,
            main.key.padding  = 0,
            key.axis.padding  = 0,
            axis.xlab.padding = 0,
            axis.top = 1.2, 
            xlab.key.padding  = 0,
            key.sub.padding   = 0,
            bottom.padding    = 0, 
            strip = 2
        ), 
        layout.widths = list(
            left.padding      = 0.5, # left
            right.padding     = 1, # right
            key.ylab.padding  = 0, # axis.y padding
            key.left          = 1.4, 
            key.right         = 1.8, 
            ylab.axis.padding = 0, # same as above
            axis.key.padding  = 0, # legend left padding
            axis.left = 1, 
            axis.right = 1
        ), 
        # axis.line = list(col = "white"),
        axis.components = list(
            left = list(pad1 = 0), 
            right = list(pad1 = 0), 
            top = list(pad1 = 0.5), 
            bottom = list(pad1 = 0.5) 
        ), 
        par.strip.text = list(cex = 2)
    )

useOuterStrips <- function (x, strip = strip.default, 
    strip.left = strip.custom(horizontal = FALSE), 
    strip.lines = 1, strip.left.lines = strip.lines) 
{
    dimx <- dim(x)
    stopifnot(inherits(x, "trellis"))
    stopifnot(length(dimx) == 2)
    as.table <- x$as.table
    opar <- if (is.null(x$par.settings)) 
        list()
    else x$par.settings
    par.settings <- modifyList(opar, 
        list(layout.heights = if (as.table) 
            list(strip = c(strip.lines, rep(0, dimx[2] - 1))) else 
            list(strip = c(rep(0, dimx[2] - 1), strip.lines)), 
                layout.widths = list(strip.left = c(strip.left.lines, rep(0, dimx[1] - 1)))
        ) 
    )
    if (is.character(strip)) 
        strip <- get(strip)
    if (is.logical(strip) && strip) 
        strip <- strip.default
    new.strip <- if (is.function(strip)) {
        function(which.given, which.panel, var.name, ...) {
            row.to.keep <- if (as.table) 
                1
            else nrow(trellis.currentLayout())
            if (which.given == 1 && current.row() == row.to.keep) 
                strip(which.given = 1, which.panel = which.panel[1], 
                  var.name = var.name[1], ...)
        }
    } else strip
    if (is.character(strip.left)) 
        strip.left <- get(strip.left)
    if (is.logical(strip.left) && strip.left) 
        strip.left <- strip.custom(horizontal = FALSE)
    
    new.strip.left <- if (is.function(strip.left)) {
        function(which.given, which.panel, var.name, ...) {
            if (which.given == 2 && current.column() == 1) 
                strip.left(which.given = 1, which.panel = which.panel[2], 
                  var.name = var.name[2], ...)
        }
    } else strip.left

    # browser()
    update(x, par.settings = par.settings, 
        strip = new.strip, strip.left = new.strip.left, 
        par.strip.text = list(lines = 0.5), 
        layout = dimx)
}
