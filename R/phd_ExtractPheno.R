Extract_Pheno <- function(t, y, nptperyear = 92, methods = c("Beck", "Elmore"), 
    return.fit = FALSE, tout = NULL) 
{
    INPUT <- check_input(t, y, nptperyear = nptperyear, 
                         # south = south, 
                         maxgap = nptperyear/4, alpha = 0.02, wmin = 0.2)
    brks <- season(INPUT, calendarYear = TRUE, IsPlot = FALSE)
    fit <- curvefits(INPUT, brks, methods = methods, print = FALSE)
    
    # plot(tout, x$zs$iter2)
    # lines(tout, y1, col = "red")
    if (!return.fit) {
        # d_fit <- get_fitting(fit)
        # p <- plot_phenofit(d_fit, brks)
        d_pheno <- get_pheno(fit)
        d_pheno
    } else {
        d_fit_new <- double_fits_years(fit)
        d_fit_new
        # merge(d_fit_new, ) 
    }
}

double_fits_years <- function(fit) {
    map(fit, double_fits) %>% 
        do.call(rbind, .)
}

# doubleLog.Beck
double_fits <- function(x) {
    data <- x$data
    t    <- data$t
    
    d_beck   <- double_fit(x$fFIT$Beck, t, doubleLog.Beck)
    d_elmore <- double_fit(x$fFIT$Elmore, t, doubleLog.Elmore)
    list(Beck = d_beck, Elmore = d_elmore) %>% melt_list("meth") %>% 
        cbind(data, .)
}

double_fit <- function(fFIT, t, FUN) { 
    # t    <- fFIT$tout
    par  <- fFIT$par[1, ]
    
    ans <- list()
    n <- nrow(pars0)
    for(i in 1:n) {
        par_new <- LSP_par(par, pars0$d_sos[i], pars0$d_eos[i])
        ans[[i]] <- FUN(par_new, t)
    }
    ans %>% set_names(1:n) %>% as.data.table()
}

# 设计13组实验
pars0 <- rbind(
    data.table(d_sos = 0, d_eos = 0),
    data.table(d_sos = c(1, 2, 5) %>% c(-rev(.), .), d_eos = 0), 
    data.table(d_sos = 0, d_eos = c(1, 2, 5) %>% c(-rev(.), .)))

LSP_par <- function(par, d_sos = 0, d_eos = 0) {
    # par <- x$par
    par["sos"] <- par["sos"] + d_sos
    par["eos"] <- par["eos"] + d_eos
    par
}

process_ExtractPheno <- function(data, dates, return.fit = FALSE) {
    mean <- rowMeans2(data, na.rm = TRUE)
    indexes = 1:nrow(data) %>% set_names(., .)
    # InitCluster(12)
    res <- foreach(i = indexes, icount()) %dopar% {
        runningId(i, 10)
        y = data[i, ]
        if (mean(y, na.rm = TRUE) <= 0.05) return(NULL)
        
        tryCatch({
            d     <- Extract_Pheno(dates, y, return.fit = return.fit) 
            I_rem <- d[, .(t, meth)] %>% {!duplicated(.)}
            
            d2 <- d[I_rem, ] %>% plyr::mutate(t = ymd("2000-01-01") + t) %>% 
                merge(data.table(t = dates), all.x = TRUE, sort = FALSE)
            # browser()
            d2
            # d$doy
        }, error = function(e) {
            message(sprintf('[e] %d: %s', i, e))
        })
    }
    res
}

get_color2 <- function (cols, n = NULL, show = FALSE) {
    # cols = rcolors[[name]]
    # if (is.null(n)) 
    #     n = length(cols)
    cols = colorRampPalette(cols)(n)
    if (show) 
        show_col(cols)
    cols
}

get_date_FromBandnames <- function(file) {
    bands <- fread(file_band)
    dates <- bands$bandname %>% gsub("\\[|\\]", "", .) %>% {strsplit(., ",")[[1]]} %>% 
        str_extract_all("\\d{4}_\\d{2}_\\d{2}") %>% unlist() %>% ymd()
    dates
}

get_modis_date <- function(dates, dn = 8) {
    date_begin = dates[1]
    date_end   = dates[length(dates)]
    
    year = year(date_begin):year(date_end)
    nptperyear = ceiling(366/dn)
    year = rep(year, each = nptperyear)
    days = seq(1, 366, dn)
    dates2 <- sprintf("%d%03d", year, days) %>% as.Date("%Y%j")
    dates2[dates2 >= date_begin & dates2 <= date_end]
}
