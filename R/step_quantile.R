step_quantile <- function(df, xname = "lai", lims = c(-4.5, 4.5), by = 0.1) {
    # df[[varname]] %>% range()
    xx  = seq(lims[1], lims[2], by)
    len = length(xx)

    xmid = ((xx[-1] + xx[-len])/2) %>% set_names(., .)
    inds = xmid %>% set_names(seq_along(.), .)

    val_xx <- df[[xname]]
    lst <- foreach(i = inds, icount()) %do% {
        runningId(i, 10)
        xmin = xx[i]
        xmax = xx[i + 1]

        I <- val_xx >= xmin & val_xx <= xmax
        d = df[I, ]
        if (nrow(d) > 0) {
            d[, sapply(.SD, quantile, probs = probs, na.rm = TRUE),
                .SDcols = c("GPP", "Ec", "Es", "Ei", "ET")] %>%
                cbind(prob = probs, .) %>% data.table()
        } else NULL
    } %>% rm_empty()

    res = melt_list(lst, xname)
    res[[xname]] %<>% as.numeric()
    res
}

plot_quantile <- function(d, xname, i = 1, fontsize= 14, xlab){
    p <- ggplot(d[is.na(alpha)], aes_string(xname, "mid")) +
        geom_ribbon(
            data = d[!is.na(alpha), ],
            aes(ymax = max, ymin = min, fill = alpha, y = NULL), alpha = 0.9
        ) +
        geom_line(color = "white") +
        facet_wrap(~variable, scale = "free_y", labeller = label_parsed,
            strip.position = "left", ncol = 1) +
        scale_fill_manual(values = cols) +
        labs(x = xlab, y = NULL) +
        scale_x_continuous(position = "top", sec.axis = sec_axis(~ .)) + 
        theme(
            axis.text = element_text(color = "black"),
            legend.position = "bottom", # c(0, 1), "none"
            # legend.justification = c(0, 1),
            legend.title = element_blank(),
            legend.margin = margin(l = 1, t = -1, r = 2),
            axis.title.y = element_blank(),
            strip.text = element_text(face = 2, size = fontsize, lineheight = 1)
        )
    if (i == 1) {
    } else {
        p <- p + theme(strip.text = element_blank())
    }
    p
    # scale_y_continuous(limits = symmetric_range)
}
