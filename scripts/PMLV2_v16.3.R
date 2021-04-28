df_org = fread("data-raw/PMLV2/PMLV2_global_IGBP_statistics-(2003-2017)_V15.csv")
df_new = fread("data-raw/PMLV2/PMLV2_global_IGBP_statistics-(2001-2020)_V16.3.csv")
df_org[, IGBP_name := IGBP_006$name[IGBP]]

rename_gsub <- function(d, pattern, replacement = "") {
    rename_with(d, ~gsub(pattern, replacement, .))
}

{
    d_mean  = df_new %>% dplyr::select(ends_with(c("mean"))) %>%
        rename_gsub("_mean")
    d_count = df_new %>% dplyr::select(ends_with(c("count")))
    
    df = cbind(df_new[, .(year, IGBP, IGBP_name)], d_mean * d_count /1e9 * 0.25) %>% 
        mutate(ET = Ec + Ei + Es, type = "v16.3")
    # df$
    df2 = rbind(
        df_org[, .(year, IGBP, IGBP_name, ET, GPP, type)], 
        df[, .(year, IGBP, IGBP_name, ET, GPP, type)])
}

{
    p <- ggplot(df2, aes(year, ET, color = type, shape = type)) + 
        geom_point() + 
        geom_line() + 
        facet_wrap(~IGBP_name, scales = "free_y")
    write_fig(p, "PMLV2_v16.3.pdf", 12, 8)
}
