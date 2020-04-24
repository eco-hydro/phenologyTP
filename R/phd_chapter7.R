sources <- c("MCD12Q2_V5", "MCD12Q2_V6", "VIPpheno_EVI2", "VIPpheno_NDVI", "MOD13C1", "SPOT", "GIMMS3g")

sources_labels <- c(
    expression("MCD12Q2"[V5]), expression("MCD12Q2"[V6]),
    expression("VIPpheno"[EVI2]),
    expression("VIPpheno"[NDVI]),
    "MOD13C1", "SPOT",
    expression("GIMMS"[3 * g])
) %>% Ipaper::char2expr()

overlap_id <- function(grid, poly_veg) {
    veg_names <- c("常绿阔叶林", "高寒草甸", "高寒草原", "高寒灌木草甸", "荒漠", "落叶阔叶林", "热带雨林", "温带草原", "温性草原")
    poly_veg@data <- data.table(name = veg_names)
    
    grid@data <- data.table(id = 1:nrow(grid))
    r <- raster::extract(raster(grid), poly_veg) %>% set_names(as.character(poly_veg$name))
    lst_id <- r %>% map(rm_empty)
    lst_id$`青藏高原总体` <- unlist(lst_id) %>% as.numeric()
    lst_id
}

pcor2 <- function(d) {
    d <- na.omit(d)
    if (nrow(d) <= 6) return(NULL)
    # set_colnames(bands)
    tryCatch({
            pcor(d)[1:2] %>%
                map(~ .[1, -1]) #%>% set_names(bands))
    }, error = function(e) {
        message(sprintf("%s", e))
        print(d)
        NULL
    })
}

melt_cbind <- function(l) {
    l %>% do.call(rbind, .) %>% as.data.table %>% cbind(I = as.numeric(names(l)), .)
}

# partial correlation of phenological metrics with phenology
corr_pheno <- function(mat, SOS, EOS) {
    bands = c("SOS", "EOS")
    dimnames <- list(bands, bands)
    ngrid <- length(grid_010.TP_cliped)
    res <- foreach(i = seq_len(ngrid) %>% set_names(., .), i = icount()) %do% {
        runningId(i, 1000)
        d = data.table(x = mat[i, ], SOS[i, ], EOS[i, ]) %>% na.omit()
        pcor2(d)
        # ans
    }
    res %>% rm_empty() %>% transpose() %>% {map(., function(x){
        cbind(I = as.numeric(names(x)), data.table(do.call(rbind, x))) 
    })}
}

clamp <- Ipaper::clamp
# tidy pcor result for visualization -------------------------------------------
tidy_list <- function(l, ngrid) {
    tidy_data <- function(x, sources_sel = NULL) {
        d <- map(x, fill_grid2, ngrid = ngrid) %>% melt_list("type_source")
        d$type_source %<>% factor(sources)
        d2 <- melt(d, c("type_source", "I"))
        d2$variable %<>% factor(c("SOS", "EOS"))
        if (!is.null(sources)) d2 <- d2[type_source %in% sources_sel]
        d2
    }
    d <- tidy_data(l$estimate, sources[5:7])
    d_mask <- tidy_data(l$p.value, sources[5:7]) %>% mutate(mask = value <= 0.1)
    colnames(d_mask)[4] <- "pvalue"
    df <- merge(d, d_mask, sort = FALSE) # order matters for levelplot2
    df
}

get_regional_mean <- function(X, Y = NULL, areas) {
    res <- foreach(ind = id_veg_010deg, icount()) %do% {
        x    <- X[ind, ] %>% colMeans2(na.rm = TRUE) %>% {scale(.,scale = FALSE)[,1]}
        x_sd <- X[ind, ] %>% colMads(na.rm = TRUE)
        if (!is.null(Y)) {
            y    <- Y[ind, ] %>% colMeans2(na.rm = TRUE) %>% {scale(.,scale = FALSE)[,1]}
            y_sd <- Y[ind, ] %>% colMads(na.rm = TRUE)
            data.table(x, x_sd, y, y_sd, size = sum(areas[ind]))
        } else {
            data.table(x, x_sd, size = sum(areas[ind]))
        }
    }
    d <- melt_list(res, "region")
    d$size <- cut(d$size/1e3, c(0, 50, 100, 200, 250, Inf)) # 10^3 km^2
    d
}

plot_region_mean <- function(df, 
    band = "GPP", 
    ylab = expression("总初级生产力变化 (" * gC ~ m^-2 ~ a^-1 * ")"), 
    outfile = "Figure7-7 GPP ~ phenology metrics.emf", 
    devices = "pdf", show = TRUE)
{
    ylab = char2expr(ylab)
    outfile = glue("Figure7-7 {band} ~ impact on phenology.emf")
    p <- ggplot(df[variable == band & region != "青藏高原总体"], aes(x2, y2)) +
        geom_point(aes(shape = region, color = region, size = size)) +
        geom_smooth(method = "lm") +
        stat_fit_tidy(
            method = "lm",
            label.x = "left", label.y = 0.02,
            geom = "label_npc", label.size = NA, size = 4,
            method.args = list(formula = y ~ x), parse = TRUE,
            mapping = aes(label = sprintf(
                'Slope~"="~%.2f ~", "*italic(p)~"="~%.3f', #  ~ mm ~ a^-1 ~ d^-1
                stat(x_estimate), stat(x_p.value) )) ) +
        scale_shape_manual(values = 0:18) +  
        scale_size_manual(values = seq(1, 10, 0.7)) +
        labs(
            size = expression("面积 (" * 10^3 ~ km * ")"),
            shape = "植被分区", color = "植被分区", fill = "植被分区",
            x = "植被物候变化 (天)", y = ylab) +
        guides(size = FALSE) +
        facet_grid(metric ~ source, scales = "free", labeller = label_parsed)
    p <- facet_tag(p, size = 5, fontsize = fontsize) +
        guides(shape = guide_legend(override.aes = list(size = 5))) +
        theme(
            legend.box.margin = margin(-5, 0, -5, -0.5, "cm"),
            plot.margin = margin(0, 0, 0.4, 0.2, "cm"),
            strip.text = element_text(family = "TimesSimSun"),
            axis.text = element_text(color = "black"),
            axis.title.x = element_text(margin = margin(0.2, 0, -0.3, 0, "cm"))
        )
    write_fig(p, outfile, 10, 6.5, use.cairo_pdf = TRUE, devices = devices, show = show)
}

# Figure 7-5, 7-6
plot_pcor_spatial <- function(df, 
    outfile = "Figure7-5 GPP pcor with phenology.pdf", devices = c("jpg", "pdf"), show = show) 
{
    df$variable %<>% factor(c("SOS", "EOS", "LOS"), c("生长季开始时间", "生长季结束时间", "生长季长度")) #%>% label_tag(tag = FALSE))
    # browser()
    stat = list(show = TRUE, name = "均值", loc = c(84.5, 25.7), digit = 1, include.sd = TRUE, FUN = weightedMean)
    stat_sign = list(loc1 = c(78, 41.5), loc2 = c(78, 41.5 - 2))
    pars = list(
        title = list(x = 74, y = 41, cex = 1.5),
        hist = list(
            origin.x = 77, origin.y = 28, A = 12, by = 0.5, box.width = 0.48, ylab.offset = 3,
            tick = seq(0, 0.3, 0.1)
        )
    )

    # df = df_GPP
    # brks = .brks$SOS
    brks = c(0.2, 0.6, 1) %>% c(-rev(.), 0, .)
    # cols = get_color("MPL_RdYlGn") %>% rev()
    cols = RColorBrewer::brewer.pal(11, "RdYlBu") #%>% rev()
    p <- levelplot2(value ~ s1 + s2 | type_source * variable, 
                    df, SpatialPixel, 
                    # df,
                    # df.mask[type_trend == "MK" & variable %in% varnames],
                    sp.layout = sp_layout,
                    # layout = c(2, 4),
                    ylim = c(25.5, 43),
                    xlim = c(73.2, 104.98),
                    colors = cols,
                    stat = stat, pars = pars,
                    stat_sign = stat_sign, 
                    brks = brks,
                    strip = TRUE,
                    # strip.factors = sources_labels,
                    panel.titles_full = label_tag(rep("", 6)),
                    density = 0.5,
                    par.shade = list(lwd = 0.5),
                    border = "white", border.lwd = 0.01,
                    legend.space = "right",
                    legend.num2factor = TRUE,
                    par.settings2 = list(strip.background = list(alpha = 0)),
                    par.strip.text = list(cex = 1.5, font = 2, fontfamily = "TimesSimSun", lineheight = 2),
                    # par.settings = opt_trellis_strip,
                    interpolate = FALSE,
                    aspect = .7) +
        theme_lattice(
            # key.margin = c(0, 1, 0, 0),
            plot.margin = c(0.2, 2.5, -1.5, 0.2))
    p %<>% useOuterStrips(
        strip = strip.custom(factor.levels = sources_labels[5:7]), # %>% label_tag()
        # strip.left = strip.custom(factor.levels = varnames_zh),
        strip.lines = 1.1)
    scale = 1
    write_fig(p, outfile, 10, 4.7, devices = devices, show = show, use.cairo_pdf = TRUE) # clear and small file size
}

#' @examples 
#' plot_pcor_spatial2(df2, "GPP", "jpg", TRUE)
plot_pcor_spatial2 <- function(df2, responsor, SpatialPixel, devices = c("jpg", "pdf"), show = show, 
    prefix = "") 
{
    ngrid <- length(SpatialPixel)
    d <- df2[response == responsor, -(2:3)]
    # c("SOS", "EOS", "yearMax", "gsMean")
    # d$variable %<>% as.character() %>% factor(c("SOS", "EOS", "yearMax", "gsMean"), 
    #     c("生长季开始时间", "生长季结束时间", "年LAI最大值", "生长季LAI均值"))
    # browser()
    indicators <- d$variable %>% unique()
    d_temp <- expand.grid(I = 1:ngrid, type_source = sources[5:7], variable = indicators) %>% data.table()
    d <- merge(d_temp, d, all.x = TRUE, sort = FALSE) #%>% summary()

    outfile = glue("Figure7-5 {prefix}{responsor} pcor with phenology.pdf")
    # df$variable %<>% factor(c("SOS", "EOS", "LOS"), c("生长季开始时间", "生长季结束时间", "生长季长度")) #%>% label_tag(tag = FALSE))
    # browser()
    stat = list(show = TRUE, name = "均值", loc = c(84.5, 25.7), digit = 2, include.sd = TRUE, FUN = weightedMean)
    stat_sign = list(loc1 = c(78, 41.5), loc2 = c(78, 41.5 - 2))
    pars = list(
        title = list(x = 74, y = 41, cex = 1.5),
        hist = list(
            origin.x = 78, origin.y = 28, A = 12, by = 0.5, box.width = 0.48, ylab.offset = 3.5,
            tick = seq(0, 0.3, 0.1)
        )
    )

    # df = df_GPP
    # brks = .brks$SOS
    brks = c(0.2, 0.6, 1) %>% c(-rev(.), 0, .)
    # browser()
    # cols = get_color("MPL_RdYlGn") %>% rev()
    cols = RColorBrewer::brewer.pal(11, "RdYlBu") #%>% rev()
    p <- levelplot2(value ~ s1 + s2 | variable * type_source , 
                    d, SpatialPixel, 
                    # df,
                    # df.mask[type_trend == "MK" & variable %in% varnames],
                    sp.layout = sp_layout,
                    # layout = c(2, 4),
                    ylim = c(25.5, 43),
                    xlim = c(73.2, 104.98),
                    colors = cols,
                    stat = stat, pars = pars,
                    stat_sign = stat_sign, 
                    brks = brks,
                    strip = TRUE,
                    # strip.factors = sources_labels,
                    panel.titles_full = label_tag(rep("", 20)),
                    density = 0.5,
                    par.shade = list(lwd = 0.5),
                    border = "white", border.lwd = 0.01,
                    legend.space = "right",
                    legend.num2factor = TRUE,
                    par.settings2 = list(strip.background = list(alpha = 0)),
                    par.strip.text = list(cex = 1.5, font = 2, fontfamily = "TimesSimSun", lineheight = 2),
                    # par.settings = opt_trellis_strip,
                    interpolate = FALSE,
                    aspect = .7) +
        theme_lattice(
            # key.margin = c(0, 1, 0, 0),
            plot.margin = c(0.2, 2.5, -1.5, 0.5))
    p %<>% useOuterStrips(
        # strip = strip.custom(factor.levels = sources_labels[5:7]), # %>% label_tag()
        # strip.left = strip.custom(factor.levels = varnames_zh),
        strip.lines = 1.1)
    scale = 1
    write_fig(p, outfile, 12, 6.3, devices = devices, show = show, use.cairo_pdf = TRUE) # clear and small file size
}

plot_pcor_spatial3 <- function(d, SpatialPixel, devices = c("jpg", "pdf"), show = show, 
    prefix = "", height = 10) 
{
    ngrid <- length(SpatialPixel)
    # d <- df2[response == responsor, -(2:3)]
    # c("SOS", "EOS", "yearMax", "gsMean")
    # d$variable %<>% as.character() %>% factor(c("SOS", "EOS", "yearMax", "gsMean"), 
    #     c("生长季开始时间", "生长季结束时间", "年LAI最大值", "生长季LAI均值"))
    # browser()
    # indicators <- d$variable %>% unique()
    # d_temp <- expand.grid(I = 1:ngrid, type_source = sources[5:7], variable = indicators) %>% data.table()
    # d <- merge(d_temp, d, all.x = TRUE, sort = FALSE) #%>% summary()

    outfile = glue("Figure7-5 {prefix}ALL pcor with phenology.pdf")
    # df$variable %<>% factor(c("SOS", "EOS", "LOS"), c("生长季开始时间", "生长季结束时间", "生长季长度")) #%>% label_tag(tag = FALSE))
    # browser()
    stat = list(show = TRUE, name = "均值", loc = c(84.5, 25.7), digit = 2, include.sd = TRUE, FUN = weightedMean)
    stat_sign = list(loc1 = c(78, 41.5), loc2 = c(78, 41.5 - 2))
    pars = list(
        title = list(x = 74, y = 41, cex = 1.5),
        hist = list(
            origin.x = 78, origin.y = 28, A = 12, by = 0.5, box.width = 0.48, ylab.offset = 3.5,
            tick = seq(0, 0.3, 0.1)
        )
    )

    # df = df_GPP
    # brks = .brks$SOS
    brks = c(0.2, 0.6, 1) %>% c(-rev(.), 0, .)
    # browser()
    # cols = get_color("MPL_RdYlGn") %>% rev()
    cols = RColorBrewer::brewer.pal(11, "RdYlBu") #%>% rev()
    
    # p <- levelplot2(value ~ s1 + s2 | variable * response , 
    #                 d, SpatialPixel, 
    #                 # df,
    #                 # df.mask[type_trend == "MK" & variable %in% varnames],
    #                 sp.layout = sp_layout)
    
    p <- levelplot2(value ~ s1 + s2 | variable * response ,
                    d, SpatialPixel,
                    # df,
                    # df.mask[type_trend == "MK" & variable %in% varnames],
                    sp.layout = sp_layout,
                    # layout = c(2, 4),
                    ylim = c(25.5, 43),
                    xlim = c(73.2, 104.98),
                    colors = cols,
                    stat = stat, pars = pars,
                    stat_sign = stat_sign,
                    brks = brks,
                    strip = TRUE,
                    # strip.factors = sources_labels,
                    panel.titles_full = label_tag(rep("", 20)),
                    density = 0.5,
                    par.shade = list(lwd = 0.5),
                    border = "white", border.lwd = 0.01,
                    legend.space = "right",
                    legend.num2factor = TRUE,
                    par.settings2 = list(strip.background = list(alpha = 0)),
                    par.strip.text = list(cex = 1.5, font = 2, fontfamily = "TimesSimSun", lineheight = 2),
                    # par.settings = opt_trellis_strip,
                    interpolate = FALSE,
                    aspect = .7) +
        theme_lattice(
            # key.margin = c(0, 1, 0, 0),
            plot.margin = c(0.2, 2.5, -1.5, 0.5))
    p %<>% useOuterStrips(
        # strip = strip.custom(factor.levels = sources_labels[5:7]), # %>% label_tag()
        # strip.left = strip.custom(factor.levels = varnames_zh),
        strip.lines = 1.1)
    scale = 1
    write_fig(p, outfile, 12, height, devices = devices, show = show, use.cairo_pdf = TRUE) # clear and small file size
}


label_tag <- function(labels, tag = TRUE, pool = NULL) {
    if (is.null(pool)) pool <- c(letters, LETTERS)
    n <- length(labels)
    
    foreach(name = labels, i = icount(), .combine = c) %do% {
        data <- list(tag = pool[i], x = name)
        if (tag) {
            eval(substitute(expression(bold("(" * tag * ")" ~ x)), data))
        } else {
            eval(substitute(expression(bold(x)), data))
        }
    }
    # sprintf("(%s) %s", letters[1:n], labels)
}


plot_phenoImpact_spatial <- function(d, SpatialPixel, devices = c("jpg", "pdf"), show = show, 
    prefix = "", height = 10, 
    brks = c(0.5, 1, 2, 5, 10, Inf)
    ) 
{
    brks %<>% c(-rev(.), 0, .)
    ngrid <- length(SpatialPixel)
    # d <- df2[response == responsor, -(2:3)]
    # c("SOS", "EOS", "yearMax", "gsMean")
    # d$variable %<>% as.character() %>% factor(c("SOS", "EOS", "yearMax", "gsMean"), 
    #     c("生长季开始时间", "生长季结束时间", "年LAI最大值", "生长季LAI均值"))
    # browser()
    # indicators <- d$variable %>% unique()
    # d_temp <- expand.grid(I = 1:ngrid, type_source = sources[5:7], variable = indicators) %>% data.table()
    # d <- merge(d_temp, d, all.x = TRUE, sort = FALSE) #%>% summary()

    outfile = glue("Figure7-5 {prefix} phenology impacts.pdf")
    # df$variable %<>% factor(c("SOS", "EOS", "LOS"), c("生长季开始时间", "生长季结束时间", "生长季长度")) #%>% label_tag(tag = FALSE))
    # browser()
    stat = list(show = TRUE, name = "u", loc = c(84, 25.7), digit = 2, include.sd = TRUE, FUN = weightedMean)
    stat_sign = list(loc1 = c(78, 41.5), loc2 = c(78, 41.5 - 2))
    pars = list(
        title = list(x = 74, y = 41, cex = 1.5),
        hist = list(
            origin.x = 78, origin.y = 28, A = 12, by = 0.5, box.width = 0.48, ylab.offset = 3.5,
            tick = seq(0, 0.3, 0.1)
        )
    )
    # df = df_GPP
    # brks = .brks$SOS
    # brks = c(0.2, 0.6, 1) %>% c(-rev(.), 0, .)
    # brks = c(0.5, 1, 2, 5, 10, Inf) %>% c(-rev(.), 0, .)

    # browser()
    # cols = get_color("MPL_RdYlGn") %>% rev()
    cols = RColorBrewer::brewer.pal(11, "RdYlBu") #%>% rev()
    # p <- levelplot2(value ~ s1 + s2 | variable * response , 
    #                 d, SpatialPixel, 
    #                 # df,
    #                 # df.mask[type_trend == "MK" & variable %in% varnames],
    #                 sp.layout = sp_layout)
    p <- levelplot2(value ~ s1 + s2 | scenario * response ,
                    d, SpatialPixel,
                    # df,
                    # df.mask[type_trend == "MK" & variable %in% varnames],
                    sp.layout = sp_layout,
                    # layout = c(2, 4),
                    ylim = c(25.5, 43),
                    xlim = c(73.2, 104.98),
                    colors = cols,
                    stat = stat, pars = pars,
                    stat_sign = stat_sign,
                    brks = brks,
                    strip = TRUE,
                    # strip.factors = sources_labels,
                    panel.titles_full = label_tag(rep("", 30), 
                                                  pool = paste(rep(letters[1:5], each = 6), 1:6, sep = "")),
                    density = 0.5,
                    par.shade = list(lwd = 0.5),
                    border = "white", border.lwd = 0.01,
                    legend.space = "right",
                    legend.num2factor = TRUE,
                    par.settings2 = list(strip.background = list(alpha = 0)),
                    par.strip.text = list(cex = 1.5, font = 2, fontfamily = "TimesSimSun", lineheight = 2),
                    # par.settings = opt_trellis_strip,
                    interpolate = FALSE,
                    aspect = .7) +
        theme_lattice(
            # key.margin = c(0, 1, 0, 0),
            plot.margin = c(0.2, 2.5, -1.5, 0.5))
    p %<>% useOuterStrips(
        # strip = strip.custom(factor.levels = sources_labels[5:7]), # %>% label_tag()
        # strip.left = strip.custom(factor.levels = varnames_zh),
        strip.lines = 1.1)
    scale = 1
    write_fig(p, outfile, 15, height, devices = devices, show = show, use.cairo_pdf = TRUE) # clear and small file size
}
