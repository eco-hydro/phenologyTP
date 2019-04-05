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


{
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

## FIGURE 3 and 4
Fig_34 = TRUE
if (Fig_34) {
    temp <- foreach(lst = lst_plsr, varname = names(lst_plsr), i = icount()) %do% {
        nyear <- ifelse(i == 1, 34 , 14)
        vjust <- ifelse(i == 1, 2.5, 4)
        hjust <- ifelse(i == 1, 2, 1.7)
        
        foreach(obj = lst, type = names(lst)) %do% {
            
            outfile <- sprintf("Figure4_PLSR_%s_%s.pdf", varname, type)
            g1 <- pls_show(obj, nyear, hjust, vjust); write_fig(g1, outfile, 12, 7)
        }
    }
}

# lst_plsr$GIMMS$SOS$VIP

## SOS VIP and MC
{
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


## SOS relative contribution
{
    devtools::load_all()
    
    d <- map(lst_plsr, function(l){
        mat <- l$SOS$attribute_change[, -1] # rm EOS
        d_perc <- abs(mat) %>% {./rowSums2(., na.rm = TRUE)*100} %>% data.table()
        d_perc$SOS
    }) %>% as.data.frame()
    
    gridclip2_10@data <- d
    
    names <- c(expression(bold("(a) GIMMS"[3*g])),
               expression(bold("(d) MCD12Q2")))
    
    pars = list(title = list(x=77, y=39, cex=1.5), 
                hist = list(origin.x=77, origin.y=28, A=15, by = 0.6))
    p <- spplot_grid(gridclip2_10, 
                     brks = c(0, 2, 5, 10, 20, 30, Inf), 
                     colors = colors$Blues,#%>% rev(), 
                     panel.title = names,
                     toFactor = T, 
                     pars = pars, ylab.offset = 2.5,
                     lgd.title = "RMSE")
    write_fig(p, "Figure7_relative_contribution.pdf", 11.2, 3)
}

# 可能还需要置信区间
Fig_s1 = TRUE
if (Fig_s1) {
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
