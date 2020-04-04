library(magrittr)
library(tidyverse)
library(purrr)
library(phenofit)
library(plyr)
library(data.table)
library(ncdf4)
library(R.matlab)
library(Ipaper)
library(sp)
library(matrixStats)
library(rTIMESAT)
library(foreach)

#' @param FUN
TSF_main <- function(mat_y, mat_qc, job_name, nptperyear = 23, nyear,
  FUN = 2, iters = 3, half_win = floor(nptperyear/4), 
  cache = TRUE, overwrite = FALSE)
{
    if (missing(job_name)) {
        job_name <- d$site[1]
    }

    # nyear <- floor(nrow(d)/nptperyear)
    npt   <- nyear * nptperyear
    # d <- d[1:npt, ]

    # mat_y  <- dcast(d, date~site, value.var = "y") %>% .[, -1] %>% as.matrix() %>% t()
    # mat_qc <- dcast(d, date~site, value.var = "SummaryQA") %>% .[, -1] %>% as.matrix() %>% t()

    # pls make sure it's complete year in input
    file_y   <- sprintf("TSM_%s_y.txt", job_name)
    file_w   <- sprintf("TSM_%s_w.txt", job_name)
    file_set <- sprintf("TSM_%s.set", job_name)

    if (!file.exists(file_y) || overwrite) {
        write_input(mat_y , file_y, nptperyear)      
    }

    if (!file.exists(file_w) || overwrite) {
        write_input(mat_qc, file_w, nptperyear)
    }

    ## 2. Update options
    options <- list(
       job_name            = job_name,
       file_y              = file_y,             # Data file list/name
       file_w              = file_w,             # Mask file list/name
       nyear_and_nptperear = c(nyear, nptperyear),      # No. years and no. points per year
       ylu                 = c(0.05, 1),     # Valid data range (lower upper)
       ## FOR MOD13 SUMMARYQA
       # qc_1                = c(1, 1, 1),     # Quality range 1 and weight
       # qc_2                = c(2, 2, 0.5),   # Quality range 2 and weight
       # qc_3                = c(3, 4, 0.2),   # Quality range 3 and weight
       ## FOR NDVI3g.v1
       qc_1                = c(0, 0, 1),     # Quality range 1 and weight
       qc_2                = c(1, 1, 0.5),   # Quality range 2 and weight
       qc_3                = c(2, 2, 0.2),   # Quality range 3 and weight
       A                   = 0,              # Amplitude cutoff value
       output_type         = c(1, 1, 0),     # Output files (1/0 1/0 1/0), 1: seasonality data, 2: smoothed time-series, 3: original time-series.
       seasonpar           = 0.5,            # Seasonality parameter (0-1)
       iters               = iters,          # No. of envelope iterations (3/2/1)
       FUN                 = 2,              # Fitting method (1/2/3): (SG/AG/DL)
       half_win            = half_win,       # half Window size for Sav-Gol.
       meth_pheno          = 1,              # (1: seasonal amplitude, 2: absolute value, 3: relative amplitude, 4: STL trend)
       trs                 = c(0.2, 0.6)     # Season start / end values
    )

    options$job_name <- job_name
    options$FUN      <- FUN

    # update setting
    opt <- update_setting(options)
    write_setting(opt, file_set)

    TSF_process(file_set) # call TSF_process.exe

    file_tts <- sprintf("%s_fit.tts", opt$job_name)
    file_tpa <- sprintf("%s_TS.tpa", opt$job_name)

    # # note: only suit for ascii
    # tidy_tts <- function(d_tts){
    #     sites <- d_tts$row %>% paste0("v", .)
    #     npt   <- ncol(d_tts) - 2
    #     d <- d_tts %>% {.[, 3:ncol(.)]} %>% as.matrix() %>% t() %>% data.frame() %>%
    #         set_colnames(sites) %>%
    #         set_rownames(NULL) #%>%
    #         # cbind(t = 1:npt, .)
    #     d
    # }

    # d_tts <- read_tts(file_tts) %>% tidy_tts()
    # # d_tpa <- read_tpa(file_tpa)

    if (!cache){
        status1 <- file.remove(c(file_tts, file_tpa, file_y, file_w, file_set))
        status2 <- dir(".", "*.ndx", full.names = T) %>% file.remove()
    }
    # list(fit = d_tts, pheno = d_tpa)
    # d_tts
}


time2date_tpa <- function(d_tpa, d_date){
    d_tpa[, `:=`(
        date_start = predict_date(d_date, time_start), 
        date_end   = predict_date(d_date, time_end), 
        date_peak  = predict_date(d_date, time_peak) 
    )]
    return(d_tpa)
}
