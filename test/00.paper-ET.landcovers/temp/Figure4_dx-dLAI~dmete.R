source("test/main_pkgs.R")
file_dat_figure4 = "INPUT/dat_Figure4_df_sm.csv"
df_sm <- fread(file_dat_figure4)
df_sm2 = df_sm[abs(lai) > 0.05, .(year, I, Aridity, Tair, Prcp, D, lai,
    GPP = GPP/lai, Ec = Ec/lai, Es = Es/lai, Ei = Ei/lai)]

mete_variables <- c("Aridity", "Tair", "Prcp", "D")
lst_lims = list(
    Aridity = c(0, 10),
    Tair    = c(0, 30),
    Prcp    = c(0, 1500),
    D       = c(-1500, 1000)
)

levs <- unique(d_prob$alpha) # %>% rev()
nlev <- length(levs)
cols = hue_pal()(nlev - 1) # %>% rev()

labs = c(
    expression(d[GPP]/ d[LAI]),
    expression(d[Ec] / d[LAI]),
    expression(d[Es] / d[LAI]),
    expression(d[Ei] / d[LAI]))

InitCluster(4)
lst <- foreach(xname = mete_variables, lims = lst_lims, i = icount()) %dopar% {
    runningId(i)
    # xname = "lai"; xlab = "Leaf area index (LAI)"
    # xlab = "Aridity index (PET/P)"
    delta = (lims[2]-lims[1])/100
    res = step_quantile(df_sm2, xname, lims, delta)
    # d$variable %>% levels

    formula <- glue("{xname} + variable + alpha ~ val_type")
    d = melt(res, c(xname, "prob")) %>%
        merge(d_prob[, .(alpha, val_type = variable, prob)]) %>%
        dcast(formula, value.var = "value")

    ind = 1:(nlev - 1)
    d$alpha %<>% factor(levs[ind], paste0((1 - levs[ind]) * 100, "% interval"))
    d$variable %<>% factor(c("GPP", "Ec", "Es", "Ei"), as.character(labs))
    d
}
names(lst) <- mete_variables
write_list2xlsx(lst, "dat5_dx-dLAI vs meteorological factors.xlsx")

library(egg)
{
    load_all()
    ps = foreach(xname = mete_variables, d = lst, i = icount()) %do% {
        p <- plot_quantile(d, xname, i)
        tag_facet(p, I_start = (i-1)*4+1, size = 5) +  theme(legend.position = "none")
    }
    lgd = ggplot_legend(p)
    g <- arrangeGrob(grobs = ps, nrow = 1, widths = c(11, 10, 10, 10), bottom = lgd)
    write_fig(g, "Figure4_dx-dlai vs dmete.pdf", 14, 7)
}
# tag_facet(p, x = Inf, hjust = 1.4, size = 5, fontface = 1, tag_pool = labs, parse = TRUE) %>%
# write_fig(p, "Figure4 dx-dLAI ~ dmete.pdf", 9, 6)
