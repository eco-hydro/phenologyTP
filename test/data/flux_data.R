plot(TP_poly)
plot(sp,add = TRUE)

r <- raster::intersect(sp, TP_poly)
# sites <- r$site %>% code_ChrVec()
sites <- c("CN-Dan", "CN-Ha2", "CN-HaM")

library(tidyverse)
df <- fread("OUTPUT/fluxsites166_FULLSET_daily_v20191216 (80%).csv")
d <- df[site %in% sites]
d$date %<>% ymd()
ggplot(d, aes(date, GPP_DT)) + geom_point() +
    facet_wrap(~site, ncol = 1, scales = "free_y") +
    labs(y = expression(GPP[DT] ~ "(" * gC ~ m^-2 ~ d^-1 * ")"))

save(d, file = "青藏高原通量站数据.rda")
