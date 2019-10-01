#' get_preseason
#' 
#' Get preseason of autumn phenology (EOS).
#' 
#' @param Tmin, Tmax Prec, Srad: daily time-series
#' @param SOS, EOS: yearly phenological metrics
#' @param dates_mete dates of mete
#' @param DateRange_pheno range of phenology data
#' @param maxDays max length of preseason。
#' 
#' @examples
#' \dontrun{
#' dates <- seq(ymd('19820101'), ymd('20151231'), by = "day")
#' info_date <- data.table(date = dates, year = year(dates), yday = yday(dates))
#' get_preseason(Tmin, Tmax, Prec, Srad, SOS, EOS, info_date)
#' }
#' @import ppcor foreach 
#' @export
get_preseason <- function(Tmin, Tmax, Prec, Srad, SOS, EOS, dates_mete, DateRange_pheno=NULL, maxDays=150) {
    maxI    <- floor(maxDays/15)
    EOS_avg <- floor(mean(EOS, na.rm = TRUE))

    varnames <- c("Tmin", "Tmax", "Prec", "Srad")

    # modified for MCD12Q2
    info_date <- data.table(date = dates_mete, year = year(dates_mete), yday = yday(dates_mete))
    df <- data.table(Tmin, Tmax, Prec, Srad) %>% set_names(varnames) %>% 
        cbind(info_date, .)

    if (!is.null(DateRange_pheno)) {
        df  <- df[date >= DateRange_pheno[1] & date <= DateRange_pheno[2], ]
    }
    
    d_pheno  <- data.table(SOS, EOS) # column vector,  = d_sos[, 1]
    I_nozero <- !(SOS == 0 & EOS == 0) # ZERO means NA: for VIP_pheno

    mat_pcor <- foreach(j = 1:maxI, .combine = "rbind" ) %do% {
        I_start <- EOS_avg - 15*j + 1
        if (I_start <= 0) { return(NULL)}
        
        d <- df[yday <= EOS_avg & yday >= I_start, lapply(.SD, mean, na.rm = T), 
                .(year), .SDcols=varnames] %>% .[, -1] %>% cbind(d_pheno)
        I_good  <- is.na(d) %>% rowSums2(na.rm=TRUE) %>% {which(. == 0 & I_nozero)}
        
        d <- d[I_good, ]
        res <- pcor(d)$estimate
        res <- res[-nrow(res), "EOS"]
    } %>% set_rownames(NULL)
    
    pcor_max <- mat_pcor %>% apply(2, which_max)
        
    d <- foreach(var = varnames, pos = pcor_max[2, ], .combine = "cbind") %do% {
        # print(pos)
        I_start <- EOS_avg - 15*pos + 1
        d <- df[yday <= EOS_avg & yday >= I_start, lapply(.SD, mean, na.rm = T), 
                .(year), .SDcols=var] %>% .[, -1]
        d
    } %>% cbind(d_pheno)
    # return
    list(pcor.max = pcor_max[1, ], preseason = pcor_max[2, ], data = d)
}
