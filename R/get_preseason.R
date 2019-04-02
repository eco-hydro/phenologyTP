#' get_preseason
#' 
#' Get preseason of autumn phenology (EOS).
#' 
#' @param Tmin, Tmax Prec, Srad: daily time-series
#' @param SOS, EOS: yearly phenological metrics
#' @param info_date date info of mete forcing, i.e. (date, year, yday)
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
get_preseason <- function(Tmin, Tmax, Prec, Srad, SOS, EOS, info_date, maxDays=150) {
    maxI    <- floor(maxDays/15)
    EOS_avg <- floor(mean(EOS, na.rm = TRUE))

    varnames <- c("Tmin", "Tmax", "Prec", "Srad")
    df <- data.table(Tmin, Tmax, Prec, Srad) %>% set_names(varnames) %>% 
        cbind(info_date, .)
    
    d_pheno <- data.table(SOS, EOS) # column vector,  = d_sos[, 1]
    mat_pcor <- foreach(j = 1:maxI, .combine = "rbind") %do% {
        I_start <- EOS_avg - 15*j + 1
        d <- df[yday <= EOS_avg & yday >= I_start, lapply(.SD, mean, na.rm = T), 
                .(year), .SDcols=varnames] %>% .[, -1] %>% cbind(d_pheno)
        I_nona <- is.na(d) %>% rowSums2(na.rm=TRUE) %>% {which(. == 0)}
        d <- d[I_nona, ]
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
