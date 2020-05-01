source("test/main_pkgs.R")

# set probs
alphas = 1 - c(0.95, 0.9, 0.8, 0.5)
d_prob = map_dfr(alphas, ~data.table(alpha = ., min = ./2, max = 1 - ./2)) %>%
    melt("alpha", value.name = "prob") %>%
    rbind(data.table(alpha = 1, variable = "mid", prob = 0.5))
probs  = sort(d_prob$prob)

file_dat_figure4 = "INPUT/dat_Figure4_df_sm.csv"
df_sm <- fread(file_dat_figure4)
df_sm2 = df_sm[abs(lai) > 0.05, .(year, I, Aridity, Tair, Prcp, D, lai,
    GPP = GPP/lai, Ec = Ec/lai, Es = Es/lai, Ei = Ei/lai, 
    ET = (Ec + Es + Ei)/lai)]

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
    expression(d[Ei] / d[LAI]),
    expression(d[ET] / d[LAI]))

labs2 = c(
    NA,
    expression(d[Delta*Ec] / d[LAI]),
    expression(d[Delta*Es] / d[LAI]),
    expression(d[Delta*Ei] / d[LAI]),
    expression(d[Delta*ET] / d[LAI]))

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
    d$variable %<>% factor(c("GPP", "Ec", "Es", "Ei", "ET"), as.character(labs))
    d
}
names(lst) <- mete_variables
write_list2xlsx(lst, "dat5_dx-dLAI vs meteorological factors.xlsx")

library(egg)
{
    # load_all()
    fontsize = 16
    lev_old <- paste0(c(95, 90, 80, 50), "% interval")
    lev_new <- paste0(c(95, 90, 80, 50), "% 区间")
    
    labs_zh <- c("干燥度指数", "年平均温度 (℃)", "年降水 (mm)", "水平衡 (mm)")
    ps = foreach(xname = mete_variables, d = lst, i = icount()) %do% {
        d <- lst[[i]]
        d$alpha %<>% mapvalues(lev_old, lev_new)
        ind = c(1, 5, 2:4); 
        d$variable %<>% factor(labs[ind], labs2[ind])
        d <- d[variable != "NA"]
        
        xlab = ifelse(xname == "D", "Prcp - PET", xname)
        p <- plot_quantile(d, xname, i, fontsize, xlab = labs_zh[i]) + 
            theme(
                panel.background = element_rect(fill = "white"),
                panel.border = element_rect(fill = NA, color = "grey60", size = 0.5),
                # strip.text = element_text(size = fontsize - 1),
                # strip.text = element_blank(),
                strip.background = element_rect(fill = "white", color = "black"),
                axis.title = element_text(size = fontsize, face = 2),
                axis.text = element_text(size = fontsize-2),
                legend.title = element_blank(),
                # element_text(size = fontsize, face = 2),
                legend.text = element_text(size = fontsize, face = 2)
                # legend.text = 
            )
        if (i == 1) lgd = ggplot_legend(p)
        p <- facet_tag(p, I_start = (i-1)*4+1, size = 5) + theme(legend.position = "none")
        p
    }
    
    g <- arrangeGrob(grobs = ps, nrow = 1, widths = c(11, 10, 10, 10), bottom = lgd)
    write_fig(g, "Figure4_dx-dlai vs dmete.pdf", 13, 7)
}
# tag_facet(p, x = Inf, hjust = 1.4, size = 5, fontface = 1, tag_pool = labs, parse = TRUE) %>%
# write_fig(p, "Figure4 dx-dLAI ~ dmete.pdf", 9, 6)
