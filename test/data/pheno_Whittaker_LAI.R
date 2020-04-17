source("test/main_pkgs.R")

{
    bands <- fread("data-raw/smoothed_LAI_010deg_TP_names.csv")
    dates <- bands$bandname %>% gsub("\\[|\\]", "", .) %>% {strsplit(., ",")[[1]]} %>% 
        str_extract_all("\\d{4}_\\d{2}_\\d{2}") %>% unlist() %>% ymd()
    I_time <- which(dates >= '2003-01-01')
    dates <- dates[I_time]
    years = year(dates)
    grps = unique(years)
}

lst   <- readGDAL("data-raw/smoothed_LAI_010deg_TP_.tif")

grid <- grid_010.TP_cliped2
data <- lst@data
id <- grid$id

{
    grid2 <- get_grid(range = c(73, 105, 25, 40), cellsize = 0.1, type = "vec")
    grid2 <- grid2[id, ]
    plot(grid2)
    
    grid2@data <- lst@data[id, ]
}

data <- set_colnames(lst@data[id, I_time]/10, as.character(dates)) %>% as.matrix()
# r <- lst[, 1]
# plot(r[id, ])

load("data-raw/lst_LAI.rda")
vec <- lst_LAI$raw$gsMean %>% do.call(cbind, .) %>% rowMeans2()

plot(data[100, ], type = "l")

# ind <- 1:91
t = dates# %>% yday()
# y = data[100, ] 
# d <- data.table(t, y)

mean <- rowMeans2(data)
indexes = 1:nrow(data) %>% set_names(., .)
InitCluster(12)
res <- foreach(i = indexes, icount()) %dopar% {
    runningId(i, 10)
    y = data[i, ]
    if (mean(y) <= 0.05) return(NULL)
    
    tryCatch({
        d <- Extract_Pheno(t, y) 
        d$doy
    }, error = function(e) {
        message(sprintf('[e] %d: %s', i, e))
    })
}

df_LAI <- transpose(rm_empty(res)) %>% melt_tree(c("meth", "I"))
df_LAI$I %<>% as.numeric()
saveRDS(df_LAI, file = "data-raw/pheno_smoothed_LAI (2003-2017).RDS")

{
    grid <- grid_010.TP_cliped2
    d <- df_LAI[, lapply(.SD, mean, na.rm = TRUE), .(I), .SDcols = colnames(df_LAI)[-(1:4)]]
    ngrid <- length(grid)
    d <- expand.grid(I = 1:ngrid) %>% data.table() %>% merge(d, all.x = TRUE)
    grid@data <- d[, -1]
}

brks <- .brks$SOS
cols <- get_color2(.colors$SOS, length(brks)-1)
spplot(grid, metric_spring, at = brks, col.regions = cols, as.table = TRUE)

# 在羌塘高原提取的植被物候信息是错误的 -----------------------------------------
brks <- .brks$EOS
cols <- get_color2(.colors$EOS, length(brks)-1)
spplot(grid, metric_autumn, at = brks, col.regions = cols, as.table = TRUE)

## lai应该保留小数点后两位数字 -------------------------------------------------

# grid.draw(p)
# tout = 1:365
# fFITs <- curvefit(y, t, tout, methods = "Beck")
# PhenoTrs(fFITs$fFIT$Beck)
# INPUT <- check_input()
