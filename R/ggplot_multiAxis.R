#' @import gtable grid
ggplot_multiAxis <- function(p1, p2, col.right = NULL, show = TRUE){
    # intersperse a copy of the bottom axes
    if (is.null(col.right)) {
        gg  = ggplot_build(p2)
        col.right = gg$data[[1]]$colour[1]
    }

    p2 <- p2 +
        scale_y_continuous(position = "right") +
        theme(
            axis.ticks.y.right = element_line(color = col.right),
            axis.title.y.right = element_text(color = col.right),
            axis.text.y.right = element_text(color = col.right))

    g1 <- p1
    g2 <- p2

    # browser()
    if (!("gtable" %in% class(p1)))  g1 <- ggplotGrob(p1)
    if (!("gtable" %in% class(p2)))  g2 <- ggplotGrob(p2)

    g1_grob_names <- map_chr(g1$grob, "name")
    g2_grob_names <- map_chr(g2$grob, "name")

    I_panel1 <- g1$grobs %>% {grep("panel", g1_grob_names)}
    I_panel2 <- g2$grobs %>% {grep("panel", g1_grob_names)}
    panels_2   <- g2$grobs[I_panel2]

    n = length(panels_2)
    for (i in seq_len(n)) {
        ind1 = I_panel1[i]
        # ind2 = I_panel2[i]
        # I = 3
        panel1 <- g1$grobs[[ind1]] #$children[[1]]
        panel2 <- panels_2[[i]]

        panel1$children %<>% gList(., panel2$children[-1]) # The first one is background
        panel1$childrenOrder <- names(panel1$children)
        # panel2$children.[-1]
        # panel2$childrenOrder <-
        # panel2$children[[1]]$children %<>% .[-1]
        g1$grobs[[ind1]] <- panel1
    }

    ## 2. find ylab-r position
    I_yr1  <- g1$layout %$% {r[grep("ylab-r", name)]} %>% unique()
    I_yr2  <- g2$layout %$% {r[grep("axis-r|ylab-r", name)]} %>% unique() # get `axis-r` and `ylab-r`

    g = g2[, I_yr2]
    # grid.newpage()
    # browser()
    # g2$grobs[[2]]$children[[3]]$gp$col #%>% show_col()
    # col = panel2$children %>% map(~.x$gp$col) %>% unlist()
    # col = g$grobs[[4]]$children[[1]]$gp$col
    x   = -0.05
    tck = 2
    lwd = 5
    gp = gpar(col = col.right, lwd = lwd)
    axis_y        = segmentsGrob(x, 0, x    , 1, gp = gp)
    axis_y_top    = segmentsGrob(x, 1, x + tck, 1, gp = gp)
    axis_y_bottom = segmentsGrob(x, 0, x + tck, 0, gp = gp)
    axis_y = gtable_add_grob(g, gList(axis_y, axis_y_top, axis_y_bottom), t = 8, l = 1) #%>% grid.draw()
    # browser()

    all <- gtable:::cbind.gtable(
        g1[, seq(max(I_yr1))],
        # g2[, I_yr2],
        axis_y,
        # rect,
        # g1[, seq(max(I_yr1)+1, ncol(g1))],
        size = "first")

    # browser()
    if (show) {
        grid.newpage()
        grid.draw(all)
    }
    all
    # layout   <- g$layout %>% mutate(vp = sprintf("%s.%d-%d-%d-%d", name, t, r, b, l)) %>% data.table()
    # vp_panel <- layout[name == "panel", vp]
    # ## 2. overlap the new panel
    # downViewport(vp_panel)
    # grid.draw(panel)
}

ggplot_multiAxis2 <- function(p1, p2, show = TRUE){
    # intersperse a copy of the bottom axes
    g1 <- p1
    g2 <- p2

    if (!("gtable" %in% class(p1)))  g1 <- ggplotGrob(p1)
    if (!("gtable" %in% class(p2)))  g2 <- ggplotGrob(p2)

    g1_grob_names <- map_chr(g1$grob, "name")
    g2_grob_names <- map_chr(g2$grob, "name")

    I_panel1 <- grep("panel", g1_grob_names)
    I_panel2 <- grep("panel", g2_grob_names)

    panel2   <- g2$grobs[I_panel2]

    n = length(panel2)
    for (i in seq_len(n)) {
        ind1 = I_panel1[i]
        ind2 = I_panel2[i]
        browser()

        g1$grobs[[ind1]]$children %<>% c(panel2[[i]]$children)
    }

    ## 2. find ylab-r position
    I_yr1  <- g1$layout %$% {r[grep("ylab-r", name)]} %>% unique()
    I_yr2  <- g2$layout %$% {r[grep("axis-r|ylab-r", name)]} %>% unique() # get `axis-r` and `ylab-r`

    # browser()
    # g = g2[, I_yr2]. note that for gtable[i, j] is different
    g = g2$grobs[I_yr2]
    # grid.newpage()
    browser()

    col = g$grobs[[4]]$children[[1]]$gp$col
    x   = -0.05
    tck = 2
    lwd = 5
    gp = gpar(col = col, lwd = lwd)
    axis_y        = segmentsGrob(x, 0, x    , 1, gp = gp)
    axis_y_top    = segmentsGrob(x, 1, x+tck, 1, gp = gp)
    axis_y_bottom = segmentsGrob(x, 0, x+tck, 0, gp = gp)
    axis_y = gtable_add_grob(g, list(axis_y, axis_y_top, axis_y_bottom), t = 7, l = 1) #%>% grid.draw()
    # browser()

    all <- gtable:::cbind.gtable(
        g1[, seq(max(I_yr1))],
        # g2[, I_yr2],
        axis_y,
        # rect,
        # g1[, seq(max(I_yr1)+1, ncol(g1))],
        size = "first")

    if (show){
        grid.newpage()
        grid.draw(all)
    }
    all
    # layout   <- g$layout %>% mutate(vp = sprintf("%s.%d-%d-%d-%d", name, t, r, b, l)) %>% data.table()
    # vp_panel <- layout[name == "panel", vp]
    # ## 2. overlap the new panel
    # downViewport(vp_panel)
    # grid.draw(panel)
}
