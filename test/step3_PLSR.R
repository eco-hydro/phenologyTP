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
    
    grps = c(4)
    lst  = lst_preseason[grps]
    lst_plsr2 <- foreach(mat_preseason = lst, var = names(lst), grp = grps, icount()) %do% {
        # if (grp != 3) return()
        res <- foreach(d = mat_preseason$data, 
                       i = icount()) %dopar% {
            Ipaper::runningId(i, 100, ngrid)
            tryCatch(phenology::plsr_attributable(d, slope = Ipaper::slope), 
                    error = function(e){
                        message(sprintf("[%-12s: %d] %s", var, i, e$message))
                    })
       }
        # fix I_rem for SPOT, 20191019
        I_rem <- mat_preseason$I[map_lgl(res, ~!is.null(.x)) %>% which()]
        res2 <- rm_empty(res) %>% purrr::transpose() %>% map(function(l){
                l2 <- transpose(l) 
                ans = l2[-6] %>% map(~do.call(rbind, .)) 
                ans$scale = l2[[6]] %>% transpose() %>% map(~do.call(rbind, .))
                ans
            })
        res2$I <- I_rem
        res2
    }
    lst_plsr$GIMMS$`Non-SOS`$Q2    
    save(lst_plsr, file = file_plsr)
} else {
    load(file_plsr)    
}

init = 0
if (init == 0) {
    init = init + 1
    source("test/main_pkgs.R")

    # load(file_plsr_mk)
    load(file_preseason)
    load(file_pheno_010)
    load("data/00basement_TP.rda")     
}


## Figure2: RMSE of PLSR in the spatial -----------------------------------------
# Figure2 = TRUE
# /test/s3_1. Check-PLSR-performance.R

# d <- foreach(l = lst_plsr, i = icount()) %do% {
#     nyear <- ifelse(i == 1, 34 , 14)
#     as.data.table(l$SOS$attribute_change) * nyear
# } %>% melt_list("type")
# d[, map(.SD, sd), .(type),.SDcols = colnames(d)[-7]]
# d[, map(.SD, mean), .(type),.SDcols = colnames(d)[-7]]

# lst_plsr$GIMMS$SOS$VIP
ngrid <- lst_plsr$GIMMS$SOS$ypred %>% nrow()
## Figure4: SOS relative contribution ------------------------------------------
Figure4 = TRUE
if (Figure4) {
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
    gridclip2_10@data <- data.frame(d)[, c(1, 4, 3, 5, 8, 7)]
    
    names <- c(expression(bold("(a) RC" [SOS]  * " (GIMMS" [3*g]*")")),
               expression(bold("(b) RC" [SOS]  * " (MCD12Q2)")),
               expression(bold("(c) RC" [SOS]  * " (SPOT)")),
               # expression(bold("(c) RC" [SOS]  * " (SPOT)")),
               expression(bold("(d) RC" [mete] * " (GIMMS" [3*g]*")")),
               expression(bold("(e) RC" [mete] * " (MCD12Q2)")),
               expression(bold("(f) RC" [mete] * " (SPOT)")) 
    )
    
    par.settings2 = list(
        layout.heights  = list(bottom.padding = 0),
        layout.widths   = list(axis.left = 0, axis.right = 0, key.left = 0, key.right = 2, axis.key.padding = -2, right.padding = 0),
        axis.components = list(left = list(pad1 = 0)),
        axis.line = list(col = "white")
    )
    
    pars = list(title = list(x=75.7, y=39.4, cex=1.5), 
                hist = list(origin.x=76.4, origin.y=28, A=10, by = 0.6, tick = c(0, 0.1, 0.2, 0.3, 0.4)))
    p <- spplot_grid(gridclip2_10, 
                     brks = c(0, 5, 10, 20, 50, 80, 90, 95, 98, Inf), 
                     colors = .colors$Blues[1:9],#%>% rev(), 
                     panel.title = names,
                     layout = c(3, 2), 
                     sp.layout = sp_layout, 
                     toFactor = T, 
                     unit = "%", unit.adj = 2, 
                     pars = pars, ylab.offset = 2.5, 
                     par.settings2 = par.settings2,
                     stat = list(show = TRUE, name="RC", unit="%", loc = c(83, 26.5)),
                     lgd.title = "RMSE")
    # write_fig(p, "Figure7_relative_contribution.pdf", 11, 5.5)
    write_fig(p, "Figures/Figure4_SOS and mete relative_contribution.tif", 15, 5.5)
}

## FigureS2 and S3: Relative contribution of all factors ------------------------------
FigureS2 = TRUE
if (FigureS2) {
    lst_perc <- map(lst_plsr, function(l){
        I <- l$I
        mat <- l$SOS$attribute_change[, -1] # rm EOS
        d_perc <- abs(mat) %>% {./rowSums2(., na.rm = TRUE)*100} %>% data.table()
        ans    <- matrix(NA, ngrid, ncol(mat)) %>% set_colnames(colnames(d_perc)) %>% data.frame()
        ans[I, ] <- d_perc
        ans
    })
    
    # attributable change
    lst_change <- foreach(l = lst_plsr, nyear = nyears) %do% {
        I <- l$I
        mat <- l$SOS$attribute_change[, ] # rm EOS
        d <- as.data.frame(mat)
        # d_perc <- abs(mat) %>% {./rowSums2(., na.rm = TRUE)*100} %>% data.table()
        ans    <- matrix(NA, ngrid, ncol(mat)) %>% set_colnames(colnames(d)) %>% data.frame()
        ans[I, ] <- d*nyear
        ans
    }
    
    grps = c(1, 4, 3)
    df_perc <- purrr::transpose(lst_perc[grps]) %>% map(as.data.frame) %>% 
        do.call(cbind, .)
    df_change <- purrr::transpose(lst_change[grps]) %>% map(as.data.frame) %>% 
        do.call(cbind, .)
    
    # names2 <- paste0(rep(c("SOS", "mete"), each = ncol(d)), "-", colnames(d))
    varnames <- c("Tmin", "Tmax", "Prec", "Srad", "SOS")
    satellites = c(
        quote(" (GIMMS" [3*g]*")"), 
        quote(" (MCD12Q2)"), 
        quote(" (SPOT)"))

    names = foreach(varname = varnames, i = icount()) %do% {
        foreach(sate = satellites, j = icount(), .combine = c) %do% {
            no = (i-1)*3 + j
            name = eval(substitute(expression(bold("("* no *") " * varname * sate)), 
                                   list(no = letters[no], varname = varname, sate = sate)))
        }
    } %>% do.call(c, .)
    varnames <- c("EOS", "Tmin", "Tmax", "Prec", "Srad", "SOS")
    names_change = foreach(varname = varnames, i = icount()) %do% {
        foreach(sate = satellites, j = icount(), .combine = c) %do% {
            no = (i-1)*3 + j
            name = eval(substitute(expression(bold("("* no *") " * varname * sate)), 
                                   list(no = letters[no], varname = varname, sate = sate)))
        }
    } %>% do.call(c, .)
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
