source("test/main_pkgs.R")

files_d <- dir("INPUT/json/PMLV2_v015/", "PMLV2_d", full.names = TRUE)[-16] %>% set_year_names()
files_s <- dir("INPUT/json/PMLV2_v015/", "PMLV2_s", full.names = TRUE)[-16] %>% set_year_names()
files_lai <- dir("INPUT/json/PMLV2_v015/", "LAI", full.names = TRUE)[-16] %>% set_year_names()

# no that 0 is water, other than UNF
get_json <- function(file) {
    x <- read_json(file)$features
    # browser()
    d = map(x, "properties")[-1] %>% map(as.data.table) %>% do.call(rbind, .)
    # d[IGBP == 0, IGBP := 17]
    d
}
# rfluxnet:::IGBPnames_006

IGBP_code2name <- function(df) {
    df$IGBP %<>% mapvalues(1:17, LCs[-1])
    df$year %<>% as.numeric()
    df[!is.na(IGBP), ]
}

df <- list(dynamic = map(files_d, get_json) %>% melt_list("year"),
    static = map(files_s, get_json) %>% melt_list("year")) %>% melt_list("type") %>%
    IGBP_code2name()
d <- df[type == "dynamic", .(area = sum(ET_count)/1e6), .(type, year, IGBP)] %>% dcast2("IGBP", "area")
write_list2xlsx(list(), )
df_lai <- map(files_lai, get_json) %>% melt_list("year") %>%
    set_colnames(c("year", "IGBP", "count", "mean", "sd")) %>%
    IGBP_code2name()
d_lai <- df_lai[, .(
    LAI = sum(mean*count) / sum(count),
    area = sum(count)/1e6), .(year, IGBP)]

ggplot(d_lai, aes(year, LAI, color = IGBP)) +
    geom_line() + facet_wrap(~IGBP, scales = "free")
ggplot(d_lai, aes(year, area, color = IGBP)) +
    geom_line() + facet_wrap(~IGBP, scales = "free")
# 10^9 ~ km^2

Figure3_1 = FALSE # global scale
if (Figure3_1) {
    d = df[!is.na(IGBP),
           .(ET = sum(ET_count*ET_mean)/1e9,
             GPP = sum(GPP_count*GPP_mean)/1e9), .(type, year, IGBP)] %>%
        plyr::mutate(WUE = GPP/ET)
    d_g <- d[, lapply(.SD, sum), .(type, year), .SDcols = c("ET", "GPP")] %>%
        melt(c("type", "year"))

    ggplot(d_g, aes(year, value, color = type)) + geom_point() + geom_line() +
        # facet_wrap(~IGBP, scales = "free") +
        facet_wrap(~variable, scales = "free") +
        ylab(expression("Annual ET ("*10^3~km^3~y^-1*")")) + xlab("Year") +
        theme()
}


{
    d = df[!is.na(IGBP),
           .(ET = sum(ET_count*ET_mean)/1e9,
             GPP = sum(GPP_count*GPP_mean)/1e9), .(type, year, IGBP)] %>%
        plyr::mutate(WUE = GPP/ET)
    d$IGBP %<>% factor(LCs_types, LCs_types_label)
    d_lai$IGBP %<>% factor(LCs_types, LCs_types_label)

    g <- plot_differ(d, "ET", d_lai, width = 3)
    write_fig(g, "Figure3_ET.pdf", 12, 5)

    g <- plot_differ(d, "GPP", width = 3)
    write_fig(g, "Figure3_GPP.pdf", 12, 5)
    write_list2xlsx(list(tbl3 = d), "dat3_dynamic and static annual variation.xlsx")
}

tag_facet <- function (p, open = "(", close = ")", tag_pool = letters,
          x = -Inf, y = Inf, hjust = -0.5, vjust = 1.5, fontface = 2,
          family = "", ...)
{
    gb <- ggplot_build(p)
    lay <- gb$layout$layout
    tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL],
                                      close), x = x, y = y)
    browser()
    p + geom_text(data = tags, aes_string(x = "x", y = "y",
                                          label = "label"), ..., hjust = hjust, vjust = vjust,
                  fontface = fontface, family = family, inherit.aes = FALSE)
        # theme(strip.text = element_blank(), strip.background = element_blank())
}

# {
#     d = df[!is.na(IGBP),
#            .(ET = sum(ET_count*ET_mean)/1e9,
#              GPP = sum(GPP_count*GPP_mean)/1e9), .(type, year, IGBP)] %>%
#         plyr::mutate(WUE = GPP/ET)

#     d_diff <- d[type == "dynamic", .(ET, GPP, WUE)] - d[type == "static", .(ET, GPP, WUE)]
#     d_diff %<>% cbind(d[, .(type = "diff", year, IGBP)], .)
#     # d = rbind(d, d_diff)
#     size = 2
#     p_ET <- ggplot(d[IGBP == "Cropland"], aes(year, ET, color = type, shape = type)) +
#         geom_point(size = size) + geom_line() +
#         facet_wrap(~IGBP, scales = "free") +
#         ylab(expression("Annual ET ("*10^3~km^3~y^-1*")")) + xlab("Year") +
#         theme(legend.position = c(0, 1),
#               legend.justification = c(0, 1),
#               legend.title = element_blank()
#         )
#          # scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Relative humidity [%]"))

#     p_GPP <- ggplot(d, aes(year, GPP, color = type, shape = type)) +
#         geom_point(size = size) + geom_line() +
#         facet_wrap(~IGBP, scales = "free") +
#         ylab(expression("Annual GPP (Pg C "*y^-1*")")) + xlab("Year") +
#         theme(legend.position = c(0, 1),
#               legend.justification = c(0, 1),
#               # legend.background = element_rect(fill = "transparent"),
#               legend.title = element_blank()
#         )
#     p_WUE <- ggplot(d, aes(year, WUE, color = type, shape = type)) +
#         geom_point(size = size) + geom_line() +
#         facet_wrap(~IGBP, scales = "free") +
#         ylab(expression("WUE (g C "*mm^-1*H[2]*"o )")) + xlab("Year") +
#         theme(legend.position = c(0, 1),
#               legend.justification = c(0, 1),
#               legend.title = element_blank()
#         )

#     g <- arrangeGrob(p_ET, p_GPP, ncol = 1)
#     write_fig(g, "Figure3_annual_variation.pdf", 10, 8)
# }

# # Figure3
# {
#     d = df[!is.na(IGBP),
#            .(ET = sum(ET_count*ET_mean)/1e9,
#              GPP = sum(GPP_count*GPP_mean)/1e9), .(type, year, IGBP)] %>%
#         plyr::mutate(WUE = GPP/ET)

#     d_diff <- d[type == "dynamic", .(ET, GPP, WUE)] - d[type == "static", .(ET, GPP, WUE)]
#     d_diff %<>% cbind(d[, .(type = "diff", year, IGBP)], .)
#     # d = rbind(d, d_diff)
#     d = d_diff

#     size = 2
#     p_ET <- ggplot(d, aes(year, ET, color = type, shape = type)) +
#         geom_point(size = size) + geom_line() +
#         facet_wrap(~IGBP, scales = "free") +
#         ylab(expression("LULC induced Annual ET ("*10^3~km^3~y^-1*")")) + xlab("Year") +
#         theme(legend.position = c(0, 1),
#               legend.justification = c(0, 1)
#               # legend.title = element_blank()
#               )

#     p_GPP <- ggplot(d, aes(year, GPP, color = type, shape = type)) +
#         geom_point(size = size) + geom_line() +
#         facet_wrap(~IGBP, scales = "free") +
#         ylab(expression("LULC induced Annual GPP (Pg C "*y^-1*")")) + xlab("Year") +
#         theme(legend.position = c(0, 1), legend.justification = c(0, 1),
#               legend.title = element_blank())
#     p_WUE <- ggplot(d, aes(year, WUE, color = type, shape = type)) +
#         geom_point(size = size) + geom_line() +
#         facet_wrap(~IGBP, scales = "free") +
#         ylab(expression("WUE (g C "*mm^-1*H[2]*"o )")) + xlab("Year") +
#         theme(legend.position = c(0, 1), legend.justification = c(0, 1),
#               legend.title = element_blank())
#     g <- arrangeGrob(p_ET, p_GPP, ncol = 1)
#     write_fig(g, "Figure4_annual_variation_diff.pdf", 10, 8)
# }
