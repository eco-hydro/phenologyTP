
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
    geom_smooth(data = d_pred, color = "red"
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
