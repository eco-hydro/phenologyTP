# 准备三种输入数据

/test/supply/Figure_s1_check spatial dist of GIMMS3g MCD12Q2 and VIPpheno_NDVI.R`：

```R
lst_pheno <- list(
    GIMMS = list(SOS = df_SOS_10deg, EOS = df_EOS_10deg) %>% map(as.matrix),
    MCD12Q2 = lst_MCD12Q2[c(1, 4)] %>% map(~.[I_grid2_10, ] %>% set_colnames(2001:2014)),
    VIP_pheno = map(lst_VIPpheno, ~.[I_grid2_10, -1] %>% set_colnames(1982:2014))
) %>% map(~set_names(., c("SOS", "EOS")))
```
