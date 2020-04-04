{
    xname = "lai"; xlab = "Leaf area index (LAI)"; lims = c(-4.5, 4.5)
    # xname = "AI" ; xlab = "Aridity index (PET/P)"; lims = c(0, 10)
    # res = step_quantile(df_sm2, xname, lims, 0.01)
    res = step_quantile(df_sm, xname, lims, 0.01)
    # d$variable %>% levels
    formula <- glue("{xname} + variable + alpha ~ val_type")
    d = melt(res, c(xname, "prob")) %>%
        merge(d_prob[,.(alpha, val_type = variable, prob)]) %>%
        dcast(formula, value.var = "value")

    levs <- unique(d$alpha) # %>% rev()
    nlev <- length(levs)
    ind = 1:(nlev - 1)
    d$alpha %<>% factor(levs[ind], paste0((1 - levs[ind])*100, "% interval"))
    cols = hue_pal()(nlev-1) # %>% rev()

    labs = c(
        expression("(a) GPP (g C" ~ m ^ -2 * y^-1 * ")"),
        expression("(b) Ec ("*mm~y^-1*")"),
        expression("(c) Es ("*mm~y^-1*")"),
        expression("(d) Ei ("*mm~y^-1*")")
    )
    # labs = c(
    #     expression("(a) " * d[GPP]/d[LAI]),
    #     expression("(b) " * d[Ec]/d[LAI]),
    #     expression("(c) " * d[Es]/d[LAI]),
    #     expression("(d) " * d[Ei]/d[LAI])
    # )
    d$variable %<>% factor(c("GPP", "Ec", "Es", "Ei"), as.character(labs))
    {
        # p <-
        p <- ggplot(d[is.na(alpha)], aes_string(xname, "mid")) +
            geom_ribbon(data = d[!is.na(alpha), ],
                        aes(ymax = max, ymin = min, fill = alpha, y = NULL), alpha = 0.9) +
            geom_line(color = "white") +
            facet_wrap(~variable, scale = "free", labeller = label_parsed) +
            scale_fill_manual(values = cols) +
            # geom_smooth() +
            # geom_abline(slope = 1) +
            labs(x = xlab, y = NULL) +
            # geom_vline(xintercept = 0.76, col = "red", lty = 2) +
            # geom_vline(xintercept = 1.35, col = "blue", lty = 2) +
            theme(
                legend.position = "bottom", #c(0, 1), "none"
                # legend.justification = c(0, 1),
                legend.title = element_blank(),
                legend.margin = margin(l = 5, t = -1, r = 5),
                strip.text = element_text(face = 2, size = 9, lineheight=0.5))
        # scale_y_continuous(limits = symmetric_range)
        # p
        write_fig(p, "Figure(abandoned)4_dx~dLAI.pdf", 9, 6)
    }
    # tag_facet(p, x = Inf, hjust = 1.4, size = 5, fontface = 1, tag_pool = labs, parse = TRUE) %>%
}
