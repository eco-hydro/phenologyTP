source("test/main_pkgs.R")
lcs <- unique(LCs) %>% rm_empty() %>% set_names(., .)
bands <- c("GPP", "Ec", "Es", "Ei") %>% set_names(., .)
InitCluster(6)

lst_lc = foreach(lc = lcs, j = icount()) %dopar% {
    runningId(j)
    df_sm$dx <- df_sm[[lc]]
    df_sm2 <- df_sm[abs(dx) > 0.05, .(year, I, Aridity, Tair, Prcp, D, lai,
        GPP = GPP / dx, Ec = Ec / dx, Es = Es / dx, Ei = Ei / dx)]

    labs = foreach(band = bands, i = icount()) %do% {
        lab = eval(substitute(expression(d[band] / d[lc * "_ratio"]), list(band = band, lc = lc)))
    }

    ans <- foreach(xname = mete_variables, lims = lst_lims, i = icount()) %do% {
        runningId(i)
        # xname = "lai"; xlab = "Leaf area index (LAI)"
        # xlab = "Aridity index (PET/P)"
        delta <- (lims[2] - lims[1]) / 100
        res <- step_quantile(df_sm2, xname, lims, delta)
        # d$variable %>% levels
        formula <- glue("{xname} + variable + alpha ~ val_type")
        d <- melt(res, c(xname, "prob")) %>%
            merge(d_prob[, .(alpha, val_type = variable, prob)]) %>%
            dcast(formula, value.var = "value")

        ind <- 1:(nlev - 1)
        d$alpha %<>% factor(levs[ind], paste0((1 - levs[ind]) * 100, "% interval"))
        # d$variable %<>% factor(c("GPP", "Ec", "Es", "Ei"), as.character(labs))
        d
    }
    ans
}

{
    # load_all()
    temp <- foreach(lc = lcs, lst = lst_lc, j = icount()) %dopar% {
        names(lst) <- mete_variables
        outfile <- glue("dat6_[{j}_{lc}]_dx-dlc_ratio vs dmete.xlsx")
        write_list2xlsx(lst, outfile)

        # labs = foreach(band = bands, i = icount()) %do% {
        #     lab = eval(substitute(expression(d[band] / d[lc * "_ratio"]),
        #         list(band = band, lc = lc))) %>% as.character()
        # }
        # ps <- foreach(xname = mete_variables, d = lst, i = icount()) %do% {
        #     d$variable %<>% factor(c("GPP", "Ec", "Es", "Ei"), as.character(labs))
        #
        #     p <- plot_quantile(d, xname, i)
        #     tag_facet(p, I_start = (i - 1) * 4 + 1, size = 5) + theme(legend.position = "none")
        # }
        # lgd <- ggplot_legend(p)
        # g <- arrangeGrob(grobs = ps, nrow = 1, widths = c(11, 10, 10, 10), bottom = lgd)
        # write_fig(g, glue("Figure5_[{j}_{lc}]_dx-dlc_ratio vs dmete.pdf"), 14, 7)
    }
}
# tag_facet(p, x = Inf, hjust = 1.4, size = 5, fontface = 1, tag_pool = labs, parse = TRUE) %>%
# write_fig(p, "Figure4 dx-dLAI ~ dmete.pdf", 9, 6)
