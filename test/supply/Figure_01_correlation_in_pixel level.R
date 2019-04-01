load(file_pheno_010_3s)

## 1. correlation 
Figure1 = FALSE # TURE
if (Figure1) {
    l_corr <- foreach(l = lst_pheno, i = icount()) %do% {
        X <- l$SOS %>% t()
        Y <- l$EOS %>% t()
        corr_matrix(X, Y)
    } %>% set_names(names(lst_pheno))

    d_corr <- melt_list(l_corr, "type")
    d_corr[, level := cut(pvalue, c(-Inf, 0.05, 0.1, 0.2, 1),
                          labels=c("P ≤ 0.05", "0.05 < P ≤ 0.1", "0.1 < P ≤ 0.2", "0.2 < P"))]
    # rm VIP_pheno
    d_corr <- d_corr[type != "VIP_pheno", ]
    d_corr_avg <- d_corr[, .(R = mean(R, na.rm = T)), .(type)][, label := sprintf("'(%s) %s'", letters[1:2], type)]
    d_corr_avg$label[1] <- "'(a) GIMMS'[3*g]"
    p <- ggplot(d_corr[!is.na(R)], aes(R, fill = level)) + 
        geom_histogram(aes(y = ..count../sum(..count..) * 100)) + 
        geom_vline(data = d_corr_avg, aes(xintercept = R), color = "red", linetype = 2) + 
        geom_vline(xintercept = 0, color = "grey30", linetype = 1) + 
        geom_text(data = d_corr_avg, aes(x = -Inf, y = Inf, label = label, fill = NULL), 
                  hjust = -0.1, vjust = 2, size = 5, parse = T) + 
        facet_wrap(~type, nrow = 1) + 
        labs(x = "Correlation (r)", y = "Percentage of pixels (%)", fill = "pvalue") + 
        theme(panel.grid = element_blank(), 
              strip.text = element_blank(), 
              legend.position = c(1.01, 1.02), 
              legend.justification = c(1, 1),
              legend.background = element_blank()
              )
    write_fig(p, "Figure1_corr_v2.pdf", 8, 3.5)
    d_corr[pvalue < 0.1, .N/ngrid*100, .(sign(R), type)]
}

## 2. inter-annual variation ---------------------------------------------------
Figure2 = TRUE # TURE
if (Figure2) {

    d <- foreach(l = lst_pheno) %do% {
        sos = l$SOS %>% colMeans2(na.rm = T)
        eos = l$EOS %>% colMeans2(na.rm = T)

        sd_sos = l$SOS %>% colSds(na.rm = T)
        sd_eos = l$EOS %>% colSds(na.rm = T)

        year = l$SOS %>% colnames() %>% str_extract("\\d{4}") %>% as.numeric()
        data.table(year = year, SOS = sos, sd_sos = sd_sos, EOS = eos, sd_eos = sd_eos)
    } %>% set_names(names(lst_pheno)) %>% melt_list("type")
    
    types <- c("GIMMS", "MCD12Q2", "VIP_pheno")
    types_new <- c("'(a) GIMMS'[3*g]", "'(b) MCD12Q2'", "'(c) VIP_pheno'")
    d_lab <- data.frame(type=types_new[1:2])
    d$type %<>% factor(levels = types, types_new)
    
    t_sos <- dlply(d, .(type), function(d) piecewise(d$SOS, d$year[1])) # %>% purrr::transpose() %>% map(~melt_list(., "type"))
    t_eos <- dlply(d, .(type), function(d) piecewise(d$EOS, d$year[1])) # %>% purrr::transpose() %>% map(~melt_list(., "type"))
    
    trend <- t_sos$GIMMS
    
    offset <- 120
    lwd <- 0.95
    # confirmed that VIP phenology is wrong
    p <- ggplot(d[type != "'(c) VIP_pheno'"], aes(year, SOS)) + 
        geom_line(size = lwd) + 
        geom_line(aes(y = EOS - offset), color = "red", size =lwd) + 
        # geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.5, fill = "grey60") + 
        # facet_wrap(~phase, scales = "free_y", ncol = 1) + 
        scale_y_continuous("SOS  (day of year)", sec.axis = sec_axis(~.+offset, name = "EOS (day of year)")) + 
        theme(axis.text.y.right = element_text(color = "red"), 
              axis.ticks.y.right = element_line(color = "red"), 
              axis.title.y.right = element_text(color = "red"), 
              # strip.text = element_text(size = 14),
              strip.text = element_blank(),
              axis.text = element_text(size = 13), 
              axis.title = element_text(size = 16)) + 
        geom_text(data = d_lab, aes(x = 1985, y = Inf, label = type), 
                  vjust = 1.5, hjust = 0, size = 5, parse = TRUE) +
        facet_wrap(~type, ncol = 1, labeller = "label_parsed") + 
        labs(x = "Year")
    # p + geom_line(data = trend$pred, aes(x, y))
    
    write_fig(p, "Figure2_annual_variation.pdf", 9, 6)
}
# d <- d[type == "GIMMS"]
# ggplot(d, aes(year, SOS)) + geom_line() + 
#     geom_abline(data = t_sos$trend[type == "GIMMS"], slope = slp, intercept = intercept)


