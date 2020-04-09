# partial correlation of phenological metrics with phenology
corr_pheno <- function(mat, SOS, EOS) {
    bands = c("SOS", "EOS")
    dimnames <- list(bands, bands)
    ngrid <- length(grid_010.TP_cliped)
    res <- foreach(i = seq_len(ngrid) %>% set_names(., .), i = icount()) %do% {
        runningId(i, 1000)
        d = data.table(x = mat[i, ], SOS[i, ], EOS[i, ]) %>% na.omit()
        if (nrow(d) <= 5) return(NULL)
            # set_colnames(bands) 
        ans <- tryCatch({
            pcor(d)[1:2] %>% 
                map(~.[1, -1] %>% set_names(bands))
        }, error = function(e) {
            message(sprintf('%s', e))
            print(d)
            NULL
        })
        ans
    }
    res %>% rm_empty() %>% transpose() %>% {map(., function(x){
        cbind(I = as.numeric(names(x)), data.table(do.call(rbind, x))) 
    })}
}

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

get_regional_mean <- function(X, Y, areas) {
    res <- foreach(ind = id_veg_010deg, icount()) %do% {
        x    <- X[ind, ] %>% colMeans2(na.rm = TRUE) %>% {scale(.,scale = FALSE)[,1]}
        x_sd <- X[ind, ] %>% colMads(na.rm = TRUE)
        y    <- Y[ind, ] %>% colMeans2(na.rm = TRUE) %>% {scale(.,scale = FALSE)[,1]}
        y_sd <- Y[ind, ] %>% colMads(na.rm = TRUE)
        data.table(x, x_sd, y, y_sd, size = sum(areas[ind]))
    }
    d <- melt_list(res, "region")
    d$size <- cut(d$size/1e3, c(0, 50, 100, 200, 250, Inf)) # 10^3 km^2
    d
}

tag_facet <- function (p, open = "(", close = ")", tag_pool = letters, 
    x = -Inf, y = Inf, hjust = -0.5, vjust = 1.5, fontface = 2, 
    family = "", ...) 
{
    gb <- ggplot_build(p)
    lay <- gb$layout$layout
    tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], 
        close), x = x, y = y)
    p + geom_text(data = tags, aes_string(x = "x", y = "y", 
        label = "label"), ..., hjust = hjust, vjust = vjust, 
        fontface = fontface, family = family, inherit.aes = FALSE)
        # theme(strip.text = element_blank(), strip.background = element_blank())
}

sources <- c("MCD12Q2_V5", "MCD12Q2_V6", "VIPpheno_EVI2", "VIPpheno_NDVI", "MOD13C1", "SPOT", "GIMMS3g")

sources_labels <- c(
    expression("MCD12Q2"[V5]), expression("MCD12Q2"[V6]),
    expression("VIPpheno"[EVI2]),
    expression("VIPpheno"[NDVI]),
    "MOD13C1", "SPOT",
    expression("GIMMS"[3 * g])
) %>% Ipaper::char2expr()
