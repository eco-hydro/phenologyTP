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
