source("test/main_pkgs.R")
load("data/00basement_TP.rda")
load(file_pheno_010)
ngrid <- length(gridclip2_10)
## PLSR result on Tibet Plateau, Dongdong Kong (20190403)
# Update: kongdd, (20191003)

par.settings2 = list(
    layout.heights  = list(bottom.padding = 0),
    layout.widths   = list(axis.left = 0, axis.right = 0, key.left = 1, key.right = 1, axis.key.padding = 1, right.padding = 1),
    axis.components = list(left = list(pad1 = 0)),
    axis.line = list(col = "white")
)


# 2.4 The difference of considering SOS or not
if (!file.exists(file_plsr)) {
    InitCluster(12)
    
    load("data/00basement_TP.rda") 
    load(file_pheno_010)
    load(file_preseason)
    ngrid <- lst_preseason$GIMMS$pcor.max %>% nrow
    
    lst_plsr <- foreach(mat_preseason = lst_preseason, var = names(lst_preseason)) %do% {
        res <- foreach(d = mat_preseason$data, 
                       i = icount(),
           .packages = c("magrittr")) %dopar% {
            Ipaper::runningId(i, 100, ngrid)
               tryCatch(phenology::plsr_attributable(d, slope = Ipaper::slope_mk), 
                        error = function(e){
                            message(sprintf("[%-12s: %d] %s", var, i, e$message))
                        })
        }
        I_rem <- mat_preseason$I[map_lgl(res, ~!is.null(.x))]
        res2 <- rm_empty(res) %>% purrr::transpose() %>% map(function(l){
                ans <- transpose(l) 
                ans %>% map(~do.call(rbind, .))
            })
        res2$I <- I_rem
        res2
    }
    lst_plsr$GIMMS$`Non-SOS`$Q2
    
    file_plsr = "OUTPUT/TP_010deg_PLSR_SOS and Non-SOS_(slope_mk).rda"
    save(lst_plsr, file = file_plsr)
} else {
    load(file_plsr)    
}

## Figure2: RMSE of PLSR in the spatial -----------------------------------------
Figure2 = TRUE
if (Figure2) {
    ## 1.1 OVERALL RMSE
    d <- map(lst_plsr, function(l){
        I <- l$I
        map(l[1:2], ~data.table(row = I, .x$Q2)) %>% melt_list("type")
    }) %>% melt_list("sate")
    
    # d$sate %<>%  factor(levels = c("GIMMS", "MCD12Q2"), c('"(a) GIMMS"[3*g]', '"(b) MCD12Q2"'))
    # g <- ggplot(d, aes(type, RMSE, fill = type)) + 
    #     stat_boxplot(geom ='errorbar', width = 0.5) +
    #     geom_boxplot2(outlier.size = -1) + 
    #     stat_summary(fun.data = label_sd, colour = "black", size = 5, geom = "text", vjust = -0.5) + 
    #     facet_wrap(~sate, labeller = label_parsed, nrow = 1) + 
    #     labs(x = NULL) + 
    #     theme(legend.position = "none", 
    #           axis.text = element_text(color = "black", size = 12), 
    #           strip.text = element_text(size = 14, 
    #                                     margin = margin(1,0,1,0)*3, lineheight = 0) 
    #     )
    # write_fig(g, "Figures/Figure6_RMSE_overall.pdf", 10, 4)

    ## 1.2 RMSE IN SPATIAL
    names <- c(expression(bold("(a) GIMMS"[3*g]*" Non-SOS")),
               expression(bold("(b) GIMMS"[3*g]*" SOS")),
               expression(bold("(c) MCD12Q2"*" Non-SOS")),
               expression(bold("(d) MCD12Q2"*" SOS")))
    d_rmse <- dcast(d, row~sate+type, value.var = "RMSE")
    gridclip2_10@data <- d_rmse[, -1] %>% data.frame()
    
    pars = list(title = list(x=77, y=39, cex=1.5), 
                hist = list(origin.x=77, origin.y=27, A=15, by = 0.6))
    stat = list(show = TRUE, name="RMSE", loc = c(84, 26), include.sd = TRUE)
    {
        names <- c(expression(bold("(a) GIMMS"[3*g])),
                   expression(bold("(d) MCD12Q2")))
        # document("E:/Research/cmip5/Ipaper")
        p <- spplot_grid(gridclip2_10[c(2, 4)], 
                         brks = c(0, 3, 4, 5, 7, 10, 15, Inf), 
                         colors = .colors$default %>% rev(), 
                         sp.layout = sp_layout,
                         panel.title = names,
                         unit = "days", 
                         unit.adj = 1.5, 
                         # layout = c(2, 4),
                         toFactor = T, 
                         pars = pars, ylab.offset = 2.5,
                         stat = stat, 
                         par.settings2 = par.settings2,
                         lgd.title = "RMSE")
        write_fig(p, "Figures/Figure4_RMSE_spatial.tif", 12, 3.2)
    }
    # 添加NSE and R2
}


## FIGURE 3: Relative Contributions
Figure3 = TRUE
if (Figure3) {
    nyears = c(34, 14, 17, 33)
    
    temp <- foreach(lst = lst_plsr, varname = names(lst_plsr), i = icount(2)) %do% {
        nyear = nyears[i]
        vjust <- ifelse(i == 1, 2.5, 4)
        hjust <- ifelse(i == 1, 2, 1.7)
        
        foreach(obj = lst[1], type = names(lst)) %do% {
            outfile <- glue("Figures/Figure4_PLSR_{varname}_{type}.pdf")
            # outfile.tif <- sprintf("Figure4_PLSR_%s_%s.tif", varname, type)
            
            g1 <- pls_show(obj, nyear, hjust, vjust)
            # write_fig(g1, outfile, 12, 7.5)
            write_fig(g1, gsub(".pdf$", ".png", outfile), 12, 7.5)
        }
        # only statistic the result of SOS model
    }
    
    d <- foreach(l = lst_plsr, i = icount()) %do% {
        nyear <- ifelse(i == 1, 34 , 14)
        as.data.table(l$SOS$attribute_change) * nyear
    } %>% melt_list("type")
    
    d[, map(.SD, sd), .(type),.SDcols = colnames(d)[-7]]
    d[, map(.SD, mean), .(type),.SDcols = colnames(d)[-7]]
}

# lst_plsr$GIMMS$SOS$VIP

## Figure7: SOS relative contribution ------------------------------------------
Figure7 = TRUE
if (Figure7) {
    d <- map(lst_plsr, function(l){
        I <- l$I
        mat <- l$SOS$attribute_change[, -1] # rm EOS
        d_perc <- abs(mat) %>% {./rowSums2(., na.rm = TRUE)*100} %>% data.table()
        ans <- I*NA
        ans[I] <- d_perc$SOS
        ans
    }) %>% as.data.frame()
    names2 <- paste0(rep(c("SOS", "mete"), each = ncol(d)), "-", colnames(d))
    d <- cbind(d, 100-d) %>% set_names(names2)#[, c(1, 3, 2, 4)]
    gridclip2_10@data <- data.frame(d)
    
    names <- c(expression(bold("(a) RC" [SOS]  * " (GIMMS" [3*g]*")")),
               expression(bold("(b) RC" [mete] * " (GIMMS" [3*g]*")")),
               expression(bold("(c) RC" [SOS]  * " (MCD12Q2)")),
               expression(bold("(d) RC" [mete] * " (MCD12Q2)")))
    
    pars = list(title = list(x=76, y=39, cex=1.5), 
                hist = list(origin.x=77, origin.y=28, A=15, by = 0.6))
    p <- spplot_grid(gridclip2_10, 
                     brks = c(0, 5, 10, 20, 50, 80, 90, 95, 98, Inf), 
                     colors = .colors$Blues[1:9],#%>% rev(), 
                     # panel.title = names,
                     layout = c(2, 4), 
                     sp.layout = sp_layout, 
                     toFactor = T, 
                     pars = pars, ylab.offset = 2.5, 
                     par.settings2 = par.settings2,
                     stat = list(show = TRUE, name="RC", unit="%", loc = c(83, 26.5)),
                     lgd.title = "RMSE")
    # write_fig(p, "Figure7_relative_contribution.pdf", 11, 5.5)
    write_fig(p, "Figures/Figure7_relative_contribution.tif", 11, 11)
}

## Figure_S3 Annual variation of simulated EOS ---------------------------------
# 可能还需要置信区间
Figure_S1 = TRUE
if (Figure_S1) {
    lst <- lst_plsr$GIMMS
    p <- expression({
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
    })
    write_fig(p, "Figures/Figure5_SOS and non-SOS model.pdf", 7, 3)
}
# https://www.maketecheasier.com/sync-onedrive-linux/

## Figure_S3: SOS VIP and MC ---------------------------------------------------
Figure_S3 = TRUE
if (Figure_S3) {
    d <- map(lst_plsr, function(l){
        I <- l$I
        VIP     <- l$SOS$VIP %>% .[, ncol(.)] # rm EOS
        MC_sign <- l$SOS$std.coefs %>% sign() %>% .[, ncol(.)] # rm EOS
        
        ans <- rep(NA, ngrid)
        ans[I] <- VIP*MC_sign
        ans
    }) %>% as.data.frame()
    
    gridclip2_10@data <- d
    devtools::load_all()
    # names <- c(expression(bold("(a) GIMMS"[3*g])),
    #            expression(bold("(d) MCD12Q2")))
    
    pars = list(title = list(x=77, y=39, cex=1.5), 
                hist = list(origin.x=77, origin.y=28, A=6, by = 0.8, axis.x.text = FALSE))
    p <- spplot_grid(gridclip2_10, 
                     brks = c(0.8, Inf) %>% c(-rev(.), .), 
                     colors = .colors$default,#%>% rev(), 
                     sp.layout = sp_layout, 
                     latout = c(2, 2), 
                     # panel.title = names,
                     toFactor = T, 
                     pars = pars, ylab.offset = 2.5,
                     par.settings2 = par.settings2,
                     lgd.title = "RMSE")
    write_fig(p, "Figures/Figure8_VIP_MC_sign.pdf", 9, 4.5)
}
