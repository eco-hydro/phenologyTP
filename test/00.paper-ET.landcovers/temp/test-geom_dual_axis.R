
lc = "Cropland"
p1 <- ggplot(d_lai[IGBP == lc], aes(year, LAI, color = IGBP)) +
    geom_line() + facet_wrap(~IGBP, scales = "free") +
    theme(legend.position = "none")
p2 <- ggplot(d_lai[IGBP == lc], aes(year, -area, color = IGBP)) +
    geom_line() +
    facet_wrap(~IGBP, scales = "free") +
    theme(legend.position = "none") +
    scale_y_continuous(position = "right")

{
    g <- ggplot_multiAxis(p1, p2)
    write_fig(g, "a.pdf")
}

