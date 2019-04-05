# setwd('..')
source("test/main_pkgs.R")
# source('../phenofit/test/load_pkgs.R')
# load('INPUT/phenology_TP_AVHRR_multi-annual.rda')
# colnames(df_pheno_avg)[1] <- 'row'


load("data/00basement_TP.rda")
load(file_preseason)
ngrid <- l_preseason$GIMMS$pcor.max %>% nrow

# 2.2 preseason pcor
l_pcor <- foreach(mat_preseason = l_preseason, j = icount()) %do%
    {
        mat_pcor <- foreach(d = mat_preseason$data, i = icount(2),
            .combine = "rbind") %do% {
            phenofit::runningId(i, 1, ngrid)
            I_nona <- is.na(d) %>% rowSums2(na.rm = TRUE) %>%
                {
                  which(. == 0)
                }
            d <- d[I_nona, ]
            res <- pcor(d)$estimate
            res <- res[-nrow(res), "EOS"]
        } #%>% set_rownames(NULL)
    } #%>% set_names(names(l_preseason))
