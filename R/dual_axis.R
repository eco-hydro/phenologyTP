# x <- d[IGBP == "Cropland"]
mutate <- plyr::mutate


#' @import data.table
plot_dual_axis <- function(x, varname = "ET", width = 24, size = 3,
    show.legend = FALSE, ylim_right = c(-0.05, 0.8))
{
    d_diff <- dcast(x, year ~ type, value.var = varname) %>% mutate(value = dynamic - static)
    
    ylim_left  = x[[varname]] %>% range()
    # ylim_right = d_diff$value %>% range()
    # browser()
    # ylim_right = c(-0.05, y_right_max)

    m <- lm(ylim_left~ylim_right)

    trans = function(x) m$coef[2]*x + m$coef[1]
    trans_back <- function(x) (x - m$coef[1])/m$coef[2]
    # d_diff$diff2 <- trans(d_diff$value)
    delta = ifelse(diff(ylim_left) <= 0.05, 0.01, 0.05)
    # origin = ylim_left[1] - ylim_right[2] - delta
    origin = predict(m, data.table(ylim_right = 0))[[1]]

    if (varname == "ET") {
        # ylab_left  = expression("Annual ET ("*10^3~km^3~y^-1*")")
        # ylab_right = expression("Dynamic - Static ("*10^3~km^3~y^-1*")")
        ylab_left = expression("全球陆地表面蒸散发 (" * 10^3 ~ km^3 ~ y^-1 * ")") %>% char2expr()
        ylab_right = expression("植被变化贡献 (Dynamic - Static, " * 10^3 ~ km^3 ~ y^-1 * ")") %>% char2expr()
    } else if (varname == "GPP") {
        ylab_left = expression("Annual GPP (Pg C " * y^-1 * ")")
        ylab_right = expression("Dynamic - Static (Pg C "*y^-1*")")
    }
# browser()
    p <- ggplot(x, aes_string("year", varname)) +
        facet_wrap(~IGBP, scales = "free") +
        theme(legend.position = c(0, 1),
              legend.justification = c(0, 1),
              axis.title = element_blank(),
              # axis.title.y = element_rect(margin = c(0, ))
              axis.ticks.y.right = element_line(color = "blue"),
              axis.text.y.right = element_text(color = "blue"),
              strip.text = element_text(face = 2, size = 18),
              plot.margin = margin(1, 4, 1, 9)*2, 
              # panel.background = element_rect(fill = "transparent", colour = "black"),
              plot.background = element_rect(fill = "transparent",colour = NA),
              # panel.background = element_rect(fill = "transparent"),
              legend.margin = margin(r = 1, t = -0.5),
              legend.title = element_blank()) +
        geom_line(aes(color = type)) +
        geom_point(size = size, aes(color = type, shape = type)) +
        geom_hline(yintercept = origin, color = "black") +
        scale_color_manual(values = c(dynamic = "green", static = "black", "TRUE" = "red", "FALSE" = "blue")) +
        scale_y_continuous(sec.axis = sec_axis(trans_back)) +
        labs(x = NULL, y = NULL)
        # labs(y = ylab_left, x = "Year") , name = ylab_right
    if (varname == "GPP") {
        p <- p + theme(legend.position = c(0, 0.50))
    }

# browser()
# scale_factor = 1 # for bar
    d_diff %<>% plyr::mutate(yend = trans(value))
    d_a = d_diff[value >= 0, ]
    d_b = d_diff[value <  0, ]
# browser()

    if (nrow(d_a) > 0) {
        p <- p + geom_segment( data = d_a,
            aes(x = year, xend = year, y = origin, yend = yend, shape = NULL, size = width),
                show.legend = FALSE, color = "blue")
    }
    if (nrow(d_b) > 0) {
        p <- p + geom_segment( data = d_b,
            aes(x = year, xend = year, y = origin, yend = yend, shape = NULL, size = width),
                show.legend = FALSE, color = "red")
    }
    if (!show.legend) {
        p <- p + theme(legend.position = "none")
    }

    g <- ggplotGrob(p)
    g$widths[c(4, 6)] <- unit(0.72, "cm")
    # browser()
    g
}

plot_differ <- function(d, varname = "ET", d_lai = NULL, ...) {
    LCs_types_label = levels(d$IGBP)
    ps <- foreach(lc = LCs_types_label, i = icount()) %do% {
        x = d[IGBP == lc, ]
        show.legend = i == 1
        ylim_right = if (i <= 4) c(-0.05, 0.8) else c(-0.05, 0.8)/10

        p <- plot_dual_axis(x, varname, ..., show.legend = show.legend, ylim_right = ylim_right)
        # browser()
        # if (!is.null(d_lai)) {
        #     p2 <- ggplot(d_lai[IGBP == lc], aes(year, LAI)) +
        #         geom_line(color = "purple") +
        #         facet_wrap(~IGBP, scales = "free") +
        #         theme(legend.position = "none")
        #     p <- ggplot_multiAxis(p, p2, show = FALSE)
        # }
        p
    }

    if (varname == "ET") {
        ylab_left  = expression("全球陆地表面蒸散发 ("*10^3~km^3~y^-1*")") %>% char2expr()
        ylab_right = expression("植被变化贡献 (" * 10^3 ~ km^3 ~ y^-1 * ")") %>% char2expr()
        # ylab_left = expression("Annual ET (" * 10^3 ~ km^3 ~ y^-1 * ")")
        # ylab_right = expression("Dynamic - Static (" * 10^3 ~ km^3 ~ y^-1 * ")")
    } else if (varname == "GPP") {
        ylab_left = expression("Annual GPP (Pg C " * y^-1 * ")")
        ylab_right = expression("Dynamic - Static (Pg C " * y^-1 * ")")
    }

    fontsize = 16
    left  <- textGrob(ylab_left , gp = gpar(fontsize = fontsize, fontface = "bold"), rot = 90)
    right <- textGrob(ylab_right, gp = gpar(fontsize = fontsize, fontface = "bold", col = "blue"), rot = 270)

    g <- arrangeGrob(grobs = ps, nrow = 2, left = left, right = right)
    # g <- ggarrange(grobs = ps, nrow = 2, left = left, right = right)
    g
}
