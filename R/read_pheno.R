#' read phenology metrics of phenofit obj
#' 
#' phenology metrics of different methods are aggregated by mean value.
#' 
#' @export
read_pheno <- function(file){
    x <- read_rds(file)
    # get the mean value of those values
    pheno <- x$pheno
    names <- pheno$doy %>% map(colnames) %>% unlist() %>% unique() 
    
    # fix RDS file reversely
    if (length(grep( "V1|v1", names)) > 0 || length(names) > 19 ){
    # if (ncol(pheno$doy[[1]]) < 19) {
        pheno <- get_pheno(x$fit, TRS = c(0.2, 0.5, 0.6))
        # fix original file
        x$pheno <- pheno
        saveRDS(x, file)
    }
    
    d <- pheno$doy %>% melt_list("meth")
    d[, lapply(.SD, mean, na.rm = TRUE), .(flag, origin), .SDcols = colnames(d)[-c(1:2, ncol(d))]]
    # d[, lapply(.SD, mean, na.rm = TRUE), .(meth), .SDcols = colnames(d)[-c(1:2, ncol(d))]]
}
