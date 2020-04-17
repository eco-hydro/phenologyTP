Extract_Pheno <- function(t, y, nptperyear = 92) {
    INPUT <- check_input(t, y, nptperyear = nptperyear, 
                         # south = south, 
                         maxgap = nptperyear/4, alpha = 0.02, wmin = 0.2)
    brks <- season(INPUT, calendarYear = TRUE, IsPlot = FALSE)
    fit <- curvefits(
        INPUT, brks,
        methods = c("Beck", "Elmore"), print = FALSE)
    # d_fit <- get_fitting(fit)
    # p <- plot_phenofit(d_fit, brks)
    d_pheno <- get_pheno(fit)
    d_pheno
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
