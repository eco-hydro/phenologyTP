# lai应该保留小数点后两位数字
source("test/main_pkgs.R")

bands <- c("GPP", "ET", "Ec", "Es", "Ei")

## 多年平均情况-----------------------------------------------------------------
files <- dir("../rPML/OUTPUT/yearly", "原序列", full.names = TRUE) %>% 
    set_names(c("Beck", "Elmore"))
l_mean <- foreach(file = files, i = icount()) %do% {
    l <- ncread(file, -1)$data %>% rPML:::add_ETsum() %>% .[bands] %>% 
        map(rowMeans2, na.rm = TRUE) %>% as.data.table()
}
d_mean <- {(l_mean$Beck + l_mean$Elmore)/2} %>% cbind(I = 1:nrow(.), .) %>% 
    melt("I", value.name = "mean", variable.name = "response")

infile <- "../rPML/OUTPUT/PMLV2_LAI_phenoExperiment_scenarios.nc"

grid_full <- get_grid(range = c(73, 105, 25, 40), cellsize=0.1, type = "vec")
grid_id   <- ncread(infile, 12)$data[[1]]
grid      <- grid_full[grid_id, ]

lst_id <- overlap_id(grid, TP_poly_veg)
d_id <- map(lst_id[-c(7, 10)], ~data.table(I = .x)) %>% melt_list("region")

scenarios <- c("EOS-1", "EOS-2", "EOS-5", "EOS+1", "EOS+2", "EOS+5", "SOS-1", "SOS-2", "SOS-5", "SOS+1", "SOS+2", "SOS+5")

scenarios_eos_id <- c(3, 2, 1, 4:6)
scenarios_eos    <- scenarios[scenarios_eos_id]
scenarios_sos_id <- c(9, 8, 7, 10:12)
scenarios_sos    <- scenarios[scenarios_sos_id]

# ngrid <- 20277
years <- 2003:2017
lst   <- ncread(infile, 1:11)$data[bands]

data  <- map(lst, ~apply_3d(.x, 3))

## 补充检验是否显著
t_test_strong <- function(x, ...) {
    tryCatch(t.test(x)$p.value, 
             error = function(e) {NA_real_})
}
data.pvalue <- map(lst, function(arr) {
    mat <- array_3dTo2d(arr)
    dim = dim(arr)
    apply(mat, 1, t_test_strong) %>% set_dim(dim[1:2])
})
df_pvalue <- map(data.pvalue, function(mat) {
    d <- set_colnames(mat, scenarios) %>% data.table() %>% cbind(I = 1:nrow(.), .) %>% 
        melt("I", variable.name = "scenario")
    d
}) %>% melt_list("response")
df_pvalue$response %<>% factor(bands, bands_zh)

data_eos <- map(data, ~.x[, scenarios_eos_id] %>% set_colnames(scenarios_eos) %>% 
                    as.data.table() %>% cbind(I = 1:nrow(.), .)) %>% 
    melt_list("response") %>% 
    melt(c("response", "I"), variable.name = "scenario")
data_sos <- map(data, ~.x[, scenarios_sos_id] %>% set_colnames(scenarios_sos) %>% 
                    as.data.table() %>% cbind(I = 1:nrow(.), .)) %>% 
    melt_list("response") %>% 
    melt(c("response", "I"), variable.name = "scenario")

df = list(SOS = data_sos, EOS = data_eos) %>% melt_list("metric") %>% 
    merge(d_mean, sort = FALSE, all.x = TRUE)
df$response %<>% factor(bands, bands_zh)

df_perc <- df %>% plyr::mutate(value = value / mean * 100)
## -----------------------------------------------------------------------------
# %% ---------------------------------------------------------------------------
grid <- grid_010.TP_cliped2
# grid@data <- l_PML$GPP %>% as.data.table()
# plot(grid)

{    
    bands = c("GPP", "ET", "Ec", "Es", "Ei")
    bands_zh = c("总初级生产力", "蒸散发", "植被蒸腾", "土壤蒸发", "顶冠截流")
    indicator = c("生长季开始时间", "生长季结束时间", "年LAI最大值", "生长季LAI均值")
    
    # df$variable %<>% as.character() %>% factor(c("SOS", "EOS", "yearMax", "gsMean"), indicator)
    devices = c("jpg", "pdf")[1]
    
    ## 2. Start of growing season ----------------------------------------------
    show = FALSE
    plot_phenoImpact_spatial(df[metric == "SOS"], grid, devices, show, prefix = "SOS scenarios", 8.6)
    plot_phenoImpact_spatial(df[metric == "EOS"], grid, devices, show, prefix = "EOS scenarios", 8.6)
    
    plot_phenoImpact_spatial(df_perc[metric == "SOS"], grid, devices, show, prefix = "SOS scenarios percentage", 8.6, brks = c(0.2, 0.5, 1, 2, 5, Inf))
    plot_phenoImpact_spatial(df_perc[metric == "EOS"], grid, devices, show, prefix = "EOS scenarios percentage", 8.6, brks = c(0.2, 0.5, 1, 2, 5, Inf))
}


save(df_perc, df_pvalue, df, file = "chp_results_PMLV2_pheno_scenario_simulations.RData")
## 补充一个变化量的百分比 ------------------------------------------------------
# 荒漠没有数据，可能是该地区LAI<=0.5

get_regional_mean2 <- function(df) {
    x <- merge(df, d_id)
    # region != "荒漠"
    ans <- x[, mean(value, na.rm = TRUE), .(response, scenario, region)] %>% dcast2("scenario", "V1")
    ans <- round(ans[, -(1:2)], 2) %>% cbind(ans[, 1:2], .)
    ans
}

get_regional_signPerc <- function(df, digit = 2) {
    df$scenario %<>% factor(scenarios2)
    x <- merge(df, d_id)
    # region != "荒漠"
    ans <- x[, sum(value <= 0.05, na.rm = TRUE)/.N, .(response, region, scenario)] %>% dcast2("scenario", "V1")
    # ans <- x[, mean(value, na.rm = TRUE), .(response, scenario, region)] %>% dcast2("scenario", "V1")
    ans <- round(ans[, -(1:2)], digit) %>% cbind(ans[, 1:2], .)
    ans
}
scenarios2 <- c("SOS-5", "SOS-2", "SOS-1", "SOS+1", "SOS+2", "SOS+5", "EOS-5", "EOS-2", "EOS-1", "EOS+1", "EOS+2", "EOS+5")

r <- listk(df, df_perc) %>% map(get_regional_mean2)
r$signPerc <- get_regional_signPerc(df_pvalue)
r <- set_names(r, c("mean", "change percentage", "significant percentage"))
write_list2xlsx(r, "tbl_chp7_pheno_scenario_simulations.xlsx")
# tbl <- get_regional_sign(d, d_id, by = c("response", "region", "variable"))




