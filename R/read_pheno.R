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

# Get spatial mean of 3 dataset.


#' read MCD12Q2 V5 vegetation phenology
#' 
#' @description 
#' 0. NumCycles	Total number of valid vegetation cycles with peak in product year	7	0	
#' 1. Greenup_1	Date when EVI2 first crossed 15% of the segment EVI2 amplitude, cycle 1. Days since Jan 1, 1970.	11138	32766	
#' 2. Greenup_2	Date when EVI2 first crossed 15% of the segment EVI2 amplitude, cycle 2. Days since Jan 1, 1970.	11138	32766	
#' 3. MidGreenup_1	Date when EVI2 first crossed 50% of the segment EVI2 amplitude, cycle 1. Days since Jan 1, 1970.	11138	32766	
#' 4. MidGreenup_2	Date when EVI2 first crossed 50% of the segment EVI2 amplitude, cycle 2. Days since Jan 1, 1970.	11138	32766	
#' 5. Peak_1	Date when EVI2 reached the segment maximum, cycle 1. Days since Jan 1, 1970.	11138	32766	
#' 6. Peak_2	Date when EVI2 reached the segment maximum, cycle 2. Days since Jan 1, 1970.	11138	32766	
#' 7. Maturity_1	Date when EVI2 first crossed 90% of the segment EVI2 amplitude, cycle 1. Days since Jan 1, 1970.	11138	32766	
#' 8. Maturity_2	Date when EVI2 first crossed 90% of the segment EVI2 amplitude, cycle 2. Days since Jan 1, 1970.	11138	32766	
#' 9. MidGreendown_1	Date when EVI2 last crossed 50% of the segment EVI2 amplitude, cycle 1. Days since Jan 1, 1970.	11138	32766	
#' 10. MidGreendown_2	Date when EVI2 last crossed 50% of the segment EVI2 amplitude, cycle 2. Days since Jan 1, 1970.	11138	32766	
#' 11. Senescence_1	Date when EVI2 last crossed 90% of the segment EVI2 amplitude, cycle 1. Days since Jan 1, 1970.	11138	32766	
#' 12. Senescence_2	Date when EVI2 last crossed 90% of the segment EVI2 amplitude, cycle 2. Days since Jan 1, 1970.	11138	32766	
#' 13. Dormancy_1	Date when EVI2 last crossed 15% of the segment EVI2 amplitude, cycle 1. Days since Jan 1, 1970.	11138	32766	
#' 14. Dormancy_2	Date when EVI2 last crossed 15% of the segment EVI2 amplitude, cycle 2. Days since Jan 1, 1970.	11138	32766
#' 
#' @details 
#' - `0.15`: Greenup, Dormancy
#' - `0.5`: MidGreenup, MidGreendown
#' - `0.9`: Maturity, Senescence
#' @export
read_MCD12Q2_V6 <- function(file) {
    ibands <- 2:15
    bands <- c(
        "Greenup_1", "Greenup_2", "MidGreenup_1", "MidGreenup_2", "Peak_1", "Peak_2",
        "Maturity_1", "Maturity_2", "MidGreendown_1", "MidGreendown_2",
        "Senescence_1", "Senescence_2", "Dormancy_1", "Dormancy_2")
    ind <- seq(1, length(ibands), 2)
    ibands <- ibands[ind]
    bands <- bands[ind] %>% gsub("_1", "", .)
    # I_V6 <- c(1, 2, 7, 8, 9, 10, 13, 14) + 1
    # ibands <- I_V6[seq(1, 8, 2)]
    x <- rgdal::readGDAL(file, silent = TRUE)[ibands]
    year <- str_extract(basename(file), "\\d{4}") %>% as.numeric()
    doy_first <- as.numeric(make_date(year, 1, 1) - 1)
    x@data[x@data == 0] <- NA
    x@data %<>% subtract(doy_first)
    x@data %>% set_colnames(bands)
}

#' read MCD12Q2 V5 vegetation phenology
#' 
#' @references 
#' https://developers.google.com/earth-engine/datasets/catalog/MODIS_MCD12Q2
#' 
#' @details 
#' - `Onset_Greenness_Increase1`:	Days since Jan 1, 2000 that correspond to vegetation greenup, mode 1	0	32766
#' - `Onset_Greenness_Increase2`:	Days since Jan 1, 2000 that correspond to vegetation greenup, mode 2	0	32766	
#' - `Onset_Greenness_Maximum1`:	Days since Jan 1, 2000 that correspond to vegetation maturity, mode 1	0	32766	
#' - `Onset_Greenness_Maximum2`:	Days since Jan 1, 2000 that correspond to vegetation maturity, mode 2	0	32766	
#' - `Onset_Greenness_Decrease1`:	Days since Jan 1, 2000 that correspond to vegetation senescence, mode 1	0	32766	
#' - `Onset_Greenness_Decrease2`:	Days since Jan 1, 2000 that correspond to vegetation senescence, mode 2	0	32766	
#' - `Onset_Greenness_Minimum1`:	Days since Jan 1, 2000 that correspond to vegetation dormancy, mode 1	0	32766	
#' - `Onset_Greenness_Minimum2`:	Days since Jan 1, 2000 that correspond to vegetation dormancy, mode 2
#' @export 
read_MCD12Q2 <- function(file) {
    bands <- c("Greenup", "Maturity", "Senescence", "Dormancy")
    rgdal::readGDAL(file, silent = TRUE)[seq(1, 8, 2)]@data %>% set_names(bands)
}

read_VIPpheno <- function(file) {
    d <- rgdal::readGDAL(file, silent = TRUE)@data %>% set_colnames(c("SOS", "POP", "EOS"))
    i_bad <- which(d$EOS == 0)
    d[i_bad, ] <- NA_integer_
    d
}

# update in 20191229
# metrics_select <- c(
#     "TRS1.sos", "TRS2.sos", "TRS5.sos", "DER.sos", "TRS6.sos", "TRS8.sos", "TRS9.sos", 
#     "TRS9.eos", "TRS8.eos", "TRS6.eos", "DER.eos", "TRS5.eos", "TRS2.eos", "TRS1.eos")
# metrics_period <- c(
#     "Starting", "Starting", "Fast-growing", "Fast-growing", "Fast-growing", "Maturity", "Maturity",
#     "Senescence", "Senescence", "Fast-senescence", "Fast-senescence", "Fast-senescence", "Ending", "Ending")
# nmetrics = length(metrics_select)/2
# metric_spring <- metrics_select[ 1:nmetrics]
# metric_autumn <- metrics_select[-(1:nmetrics)]

# "DER.pop",
metric_all = c("TRS2.sos", "TRS2.eos", "TRS5.sos", "TRS5.eos", "TRS6.sos", "TRS6.eos", "DER.sos", "DER.pop","DER.eos", "UD", "SD", "DD", "RD", "Greenup", "Maturity", "Senescence", "Dormancy")
metrics_select = c("TRS2.sos", "TRS2.eos", "TRS5.sos", "TRS5.eos", "TRS6.sos", "TRS6.eos", "DER.sos", "DER.eos", "UD", "SD", "DD", "RD", "Greenup", "Maturity", "Senescence", "Dormancy")

metric_spring <- c("TRS2.sos", "TRS5.sos", "TRS6.sos", "DER.sos", "UD", "SD", "Greenup", "Maturity")
metric_autumn <- setdiff(metrics_select, metric_spring)
