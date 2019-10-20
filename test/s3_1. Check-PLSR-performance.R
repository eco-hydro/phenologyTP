init = 0
if (init == 0) {
    init = init + 1
    source("test/main_pkgs.R")

    # load(file_plsr_mk)
    load(file_preseason)
    load(file_pheno_010)
    load("data/00basement_TP.rda")     
}

# GOF of PLSR SOS model
# df_pred <- foreach(l_preseason = lst_preseason, l_plsr = lst_plsr, i = icount()) %do% {
#     temp = foreach(d = l_preseason$data, ypred = t(l_plsr$SOS$ypred), j = icount()) %do% {
#         runningId(j, 1000)
#         GOF(d$EOS, ypred, include.r = TRUE)
#     } 
#     do.call(rbind, temp) %>% data.table()
# }

info <- foreach(l_preseason = lst_preseason, l_plsr = lst_plsr, i = icount()) %do% {
    d_m <- match2(l_plsr$I, l_preseason$I) # match
    temp = foreach(ypred = t(l_plsr$SOS$ypred[d_m$I_x, ]), 
                   d = l_preseason$data[d_m$I_y], j = icount()) %do% {
        runningId(j, 1000)
        GOF(d$EOS, ypred, include.r = TRUE)
    } 
    do.call(rbind, temp) %>% data.table() %>% cbind(I = d_m$x, .)
}
d_SPOT <- info$GIMMS * NA
d_SPOT[info$SPOT$I, ] <- info$SPOT
info$SPOT <- d_SPOT

r <- info[1:3] %>% map(~.[, .(I, RMSE, NSE, MAE, R, R2)]) %>% 
    purrr::transpose() %>% map(as.data.table)

{
    pars = list(title = list(x=76, y=39.5, cex=1.2), 
        hist = list(origin.x=77, origin.y=28, A=10, by = 0.6))
    par.settings2 = list(
        layout.heights  = list(bottom.padding = 0, top.padding = 0.5),
        layout.widths   = list(axis.left = 0, axis.right = 0, key.left = 0.5, key.right = 0.7, axis.key.padding = 1, right.padding = 2),
        axis.components = list(left = list(pad1 = 0)),
        axis.line = list(col = "white")
    )
    .brks = list(
        RMSE = c(0, 3, 4, 5, 7, 10, 15, Inf),
        MAE  = c(0, 3, 4, 5, 7, 10, 15, Inf), 
        NSE  = c(-1, seq(0, 0.7, 0.1), 1), 
        R2   = c(0, seq(0.1, 0.7, 0.1), 1))
    
    indexes = c("RMSE", "MAE", "R2") %>% set_names(., .)
    ps = foreach(index = indexes, i = icount()) %do% {
        unit = ifelse(i < 3, "days", "")
         params <- list(
            brks = .brks[[index]], 
            colors = .colors$default %>% rev(), 
            sp.layout = sp_layout,
            panel.title = names,
            unit = unit, 
            unit.adj = 100, 
            # layout = c(2, 4),
            ylim = c(25.99376, 40.52632),
            toFactor = T, 
            # colorkey = FALSE, 
            pars = pars, ylab.offset = 2.5,
            stat = stat, 
            par.settings2 = par.settings2,
            lgd.title = "RMSE"
        )
        
        gridclip2_10@data <- r[[index]] %>% data.frame()
        names <- c(expression(bold("(a) GIMMS"[3*g])),
                   expression(bold("(b) MCD12Q2")))
        
        # document("E:/Research/cmip5/Ipaper")
        if (index == "R2") index = quote(R^2)
        stat = list(show = TRUE, name=index, loc = c(84, 26), digit = 1, include.sd = TRUE)
        if (i >= 3) {
            params$colors = RColorBrewer::brewer.pal(11, "RdYlGn")
            stat$digit    = 2
        }
        
        title1 = eval(substitute(
            expression(bold("(" * num*") " * index * " of GIMMS"[3*g])), list(num = letters[i*2-1], index = index) ))
        title2 = eval(substitute(
            expression(bold("("* num*") " * index * " of MCD12Q2")), list(num = letters[i*2], index = index) ))
        title3 = eval(substitute(
            expression(bold("("* num*") " * index * " of SPOT")), list(num = letters[i*3], index = index) ))
        
        # if (i == 3) params$brks = get_R2_brks(34)
        params$stat        = stat
        params$panel.title = title1
        p1 <- do.call(spplot_grid, c(gridclip2_10[, 1], params))

        # if (i == 3) params$brks = get_R2_brks(14)
        params$panel.title = title2
        p2 <- do.call(spplot_grid, c(gridclip2_10[, 2], params))
        
        # params$colorkey = TRUE
        params$panel.title = title3
        p3 <- do.call(spplot_grid, c(gridclip2_10[, 3], params))
        
        # write_fig(p, glue("Figures/Figure4_{index}_spatial2.tif"), 13.2, 3.2)
        p <- arrangeGrob(p1, p2, p3, nrow = 1, widths = c(10, 10, 10))
        p
    }

    p2 <- do.call(arrangeGrob, list(grobs = ps, ncol = 1))
    write_fig(p2, "Figure2_PLSR_model_performance.tif", 14, 7.5)
}

# 导出到 检查一下特殊点的情况



# MODIS and AVHRR were resampled into 0.1 deg
mat_obs <- df_EOS_10deg %>% as.matrix()
mat_pred <- lst_plsr$GIMMS$SOS$ypred

yobs  <- colMeans2(mat_obs, na.rm = TRUE)
ypred <- colMeans2(mat_pred, na.rm = TRUE)

d_obs  <- mat2df(mat_obs)
d_pred <- mat2df(mat_pred)
d <- list(OBS = d_obs, PRED = d_pred) %>% melt_list("type")

lst_info <- foreach(i = 1:nrow(mat_obs)) %do% {
    yobs <- mat_obs[i, ]
    ypred <- mat_pred[i, ]
    GOF(yobs, ypred, include.r = TRUE)
}

info <- do.call(rbind, lst_info) %>% data.table() %>% cbind(I = 1:nrow(.), .)

years <- 1982:2015

mat2df <- function(mat) {
    d  <- mat %>% set_colnames(years) %>% cbind(I = 1:nrow(.), .) %>% data.table() %>% melt("I")
    d$variable %<>% gsub("X", "", .) %>% as.numeric()
    d
}

ggplot(d, aes(variable, value, color = type)) + 
    # geom_smooth() + 
    # stat_summary(fun.data = stat_quantile, size = 0.6, geom = "errorbar") + 
    stat_summary(fun.data = stat_quantile, size = 1, geom = "line") + 
    geom_smooth(data = d_pred, color = "red")
#  %>% plot(type = "b")
# colMeans2(mat_pred, na.rm = TRUE) %>% lines(type = "b", col = "red")

## -----------------------------------------------------------------------------
s1_statistic = TRUE
if (s1_statistic) { 
    # check Contrasting influence of Tmin and Tmin
    map(lst_plsr, function(x){
        d <- x$SOS$std.coefs %>% data.table()
        r <- d[, .(sign(Tmin), sign(Tmax))] %>% {table(.)/ngrid} %>% as.numeric()
        c(same = sum(r[c(1, 4)]), "diff"=sum(r[2:3]))
    }) %>% do.call(rbind, .)
    
    # contribution of (Tmin + Tmax)
    map(lst_plsr, function(x){
        d <- x$SOS$attribute_change %>% data.table()
        r <- d[, -1] %>% as.matrix() %>% abs() %>% {./rowSums2(.)} %>% as.data.table()
        # ldply(r*100, label_sd) # use percentile 更合适
        {r[, Tmin+Tmax]*100} %>% label_sd()
        # r <- d[, .(sign(Tmin), sign(Tmax))] %>% {table(.)/ngrid} %>% as.numeric()
        # c(same = sum(r[c(1, 4)]), "diff"=sum(r[2:3]))
    }) %>% do.call(rbind, .)
}
