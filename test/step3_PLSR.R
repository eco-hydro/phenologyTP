source("test/main_pkgs.R")
load("data/00basement_TP.rda")
load(file_pheno_010)
ngrid <- length(gridclip2_10)
## PLSR result on Tibet Plateau, Dongdong Kong (20190403)
# 2.4 The difference of considering SOS or not

if (!file.exists(file_plsr)) {
    InitCluster(6)
    
    load("data/00basement_TP.rda") 
    load(file_pheno_010)
    load(file_preseason)
    ngrid <- l_preseason$GIMMS$pcor.max %>% nrow
    
    lst_plsr <- foreach(mat_preseason = l_preseason) %do% {
        foreach(d = mat_preseason$data, 
               i = icount(),
               .combine = , 
               .packages = c("magrittr", "phenology")) %dopar% {
                
                phenofit::runningId(i, 100, ngrid)
                plsr_attributable(d)
                # need predictions
            } %>% 
            purrr::transpose() %>% map(function(l){
                ans <- transpose(l) 
                ans %>% map(~do.call(rbind, .))
            })
    }
    lst_plsr$GIMMS$`Non-SOS`$Q2
    save(lst_plsr, file = file_plsr)
} else {
    load(file_plsr)    
}

##
{ 
    # check Contrasting influence of Tmin and Tmin
    map(lst_plsr, function(x){
        d <- x$SOS$std.coefs %>% data.table()
        r <- d[, .(sign(Tmin), sign(Tmax))] %>% {table(.)/ngrid} %>% as.numeric()
        c(same = sum(r[c(1, 4)]), "diff"=sum(r[2:3]))
    }) %>% do.call(rbind, .)
    
    # contribute
    map(lst_plsr, function(x){
        d <- x$SOS$attribute_change %>% data.table()
        r <- d[, -1] %>% as.matrix() %>% abs() %>% {./rowSums2(.)} %>% as.data.table()
        {r[, Tmin+Tmax]*100} %>% label_sd()
        # r <- d[, .(sign(Tmin), sign(Tmax))] %>% {table(.)/ngrid} %>% as.numeric()
        # c(same = sum(r[c(1, 4)]), "diff"=sum(r[2:3]))
    }) %>% do.call(rbind, .)
}


## Fig_34: RMSE of PLSR in the spatial -----------------------------------------
Fig_34 = TRUE
if (Fig_34) {
    ## 1.1 OVERALL RMSE
    d <- map(lst_plsr, function(l){
        map(l, ~data.table(row = 1:ngrid, .x$Q2)) %>% melt_list("type")
    }) %>% melt_list("sate")
    
    d$sate %<>%  factor(levels = c("GIMMS", "MCD12Q2"), c('"(a) GIMMS"[3*g]', '"(b) MCD12Q2"'))
    g <- ggplot(d, aes(type, RMSE, fill = type)) + 
        stat_boxplot(geom ='errorbar', width = 0.5) +
        geom_boxplot2(outlier.size = -1) + 
        stat_summary(fun.data = label_sd, colour = "black", size = 5, geom = "text", vjust = -0.5) + 
        facet_wrap(~sate, labeller = label_parsed) + 
        labs(x = NULL) + 
        theme(legend.position = "none", 
              axis.text = element_text(color = "black", size = 12), 
              strip.text = element_text(size = 14, 
                                        margin = margin(1,0,1,0)*3, lineheight = 0) 
        )
    write_fig(g, "Figure6_RMSE_overall.pdf", 8, 4)

    ## 1.2 RMSE IN SPATIAL
    names <- c(expression(bold("(a) GIMMS"[3*g]*" Non-SOS")),
               expression(bold("(b) GIMMS"[3*g]*" SOS")),
               expression(bold("(c) MCD12Q2"*" Non-SOS")),
               expression(bold("(d) MCD12Q2"*" SOS")))
    d_rmse <- dcast(d, row~sate+type, value.var = "RMSE")
    gridclip2_10@data <- d_rmse[, -1] %>% data.frame()
    
    pars = list(title = list(x=77, y=39, cex=1.5), 
                hist = list(origin.x=77, origin.y=28, A=15, by = 0.6))
    p <- spplot_grid(gridclip2_10, 
                     brks = c(0, 3, 4, 5, 7, 10, 15, Inf), 
                     colors = colors$default %>% rev(), 
                     panel.title = names,
                     toFactor = T, 
                     pars = pars, ylab.offset = 2.5,
                     lgd.title = "RMSE")
    write_fig(p, "Figure7_RMSE_spatial.pdf", 10, 5)
}

## FIGURE 5 and 6
Fig_56 = TRUE
if (Fig_56) {
    temp <- foreach(lst = lst_plsr, varname = names(lst_plsr), i = icount()) %do% {
        nyear <- ifelse(i == 1, 34 , 14)
        vjust <- ifelse(i == 1, 2.5, 4)
        hjust <- ifelse(i == 1, 2, 1.7)
        
        foreach(obj = lst, type = names(lst)) %do% {
            outfile <- sprintf("Figure4_PLSR_%s_%s.pdf", varname, type)
            g1 <- pls_show(obj, nyear, hjust, vjust); write_fig(g1, outfile, 12, 7)
        }
        # only statistic the result of SOS model
    }
    
    d <- foreach(l = lst_plsr, i = icount()) %do% {
        nyear <- ifelse(i == 1, 34 , 14)
        as.data.table(l$SOS$attribute_change) * nyear
    } %>% melt_list("type")
    
    d[, map(.SD, sd), .(type),.SDcols = colnames(d)[-7]]
    d[, map(.SD, mean), .(type),.SDcols = colnames(d)[-7]]
    
    browser()
}

# lst_plsr$GIMMS$SOS$VIP

## Figure7: SOS relative contribution ------------------------------------------
Figure7 = TRUE
if (Figure7) {
    devtools::load_all()
    
    d <- map(lst_plsr, function(l){
        mat <- l$SOS$attribute_change[, -1] # rm EOS
        d_perc <- abs(mat) %>% {./rowSums2(., na.rm = TRUE)*100} %>% data.table()
        d_perc$SOS
    }) %>% as.data.frame()
    d <- cbind(d, 100-d)[, c(1, 3, 2, 4)]
    gridclip2_10@data <- data.frame(d)
    
    names <- c(expression(bold("(a) RC of SOS " * "(GIMMS"[3*g]*")")),
               expression(bold("(b) RC of mete " * "(GIMMS"[3*g]*")")),
               expression(bold("(c) RC of SOS (MCD12Q2)")),
               expression(bold("(d) RC of mete (MCD12Q2)")))
    
    pars = list(title = list(x=76, y=39, cex=1.5), 
                hist = list(origin.x=77, origin.y=28, A=15, by = 0.6))
    p <- spplot_grid(gridclip2_10, 
                     brks = c(0, 5, 10, 20, 50, 80, 90, 95, 98, Inf), 
                     colors = colors$Blues[1:9],#%>% rev(), 
                     panel.title = names,
                     toFactor = T, 
                     pars = pars, ylab.offset = 2.5, 
                     stat = list(show = TRUE, name="RC", unit="%", loc = c(83, 26.5)),
                     lgd.title = "RMSE")
    write_fig(p, "Figure7_relative_contribution.pdf", 11, 5.5)
}

## Figure_S3 Annual variation of simulated EOS ---------------------------------
# 可能还需要置信区间
Figure_S1 = TRUE
if (Figure_S1) {
    lst <- lst_plsr$GIMMS
    Cairo::CairoPDF("Figure5_SOS and non-SOS model.pdf", 7, 3)
    par(mar = c(3, 3, 1, 1), mgp = c(1.8, 0.6, 0))
    
    lwd <- 1.5
    Year <- 1982:2015
    df_EOS_10deg %>% as.matrix() %>% colMeans2(na.rm = T) %>% 
        plot(Year, ., col = "black", type = "b", pch = 21, bg = "grey80", lwd = lwd, 
             ylab = "EOS (day of year)", font.lab = 2, cex.axis = 1, cex.lab=1); grid()
    lst$SOS$ypred %>% colMeans2(na.rm = T) %>% lines(Year, ., col = "blue", lwd = lwd); grid()
    lst$`Non-SOS`$ypred %>% colMeans2(na.rm = T) %>% lines(Year, ., col = "red", lwd = lwd); grid()
    legend("topleft", c("Observation", "SOS model", "Non-SOS model"), lty = 1, 
           col = c("black", "blue", "red"), pch = c(1, NA, NA))
    dev.off()
}
# https://www.maketecheasier.com/sync-onedrive-linux/

## Figure_S3: SOS VIP and MC ---------------------------------------------------
Figure_S3 = FALSE
if (Figure_S3) {
    devtools::load_all()
    d <- map(lst_plsr, function(l){
        VIP <- l$SOS$VIP %>% .[, ncol(.)] # rm EOS
        MC_sign  <- l$SOS$std.coefs %>% sign() %>% .[, ncol(.)] # rm EOS
        VIP*MC_sign
    }) %>% as.data.frame()
    
    gridclip2_10@data <- d
    devtools::load_all()
    names <- c(expression(bold("(a) GIMMS"[3*g])),
               expression(bold("(d) MCD12Q2")))
    
    pars = list(title = list(x=77, y=39, cex=1.5), 
                hist = list(origin.x=77, origin.y=28, A=6, by = 0.8, axis.x.text = FALSE))
    p <- spplot_grid(gridclip2_10, 
                     brks = c(0.8, Inf) %>% c(-rev(.), .), 
                     colors = colors$default,#%>% rev(), 
                     panel.title = names,
                     toFactor = T, 
                     pars = pars, ylab.offset = 2.5,
                     lgd.title = "RMSE")
    write_fig(p, "Figure8_VIP_MC_sign.pdf", 11.2, 3)
}
