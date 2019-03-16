

# autumn phenology model

t_start, T_base, GDD # growing degree

t_end = EOS
dates <- yearlyDates(year)


I_eos <- which(dates == t_end)
GDD = cumsum(T_base[I_eos:1] - T_mean)


model_EOS_3p <- function(Tavg, T_base, t_start, GDD_trs){
    n <- length(Tavg)

    GDD <- pmax(T_base - Tavg[t_start:n], 0) %>% cumsum()
    EOS <- which(GDD >= GDD_trs)[1] + t_start - 1

    return(EOS)
}

#' @param Tday the daily maximum temperature
#' @param Tnight the daily maximum temperature
#' 
#' @param T_base [15, 30]
#' @param T_start [1 July, EOS]
#' @param GDD_trs [0, 1500]
#' @param k [-1, 2]
#' 
#' @export
model_EOS_wu <- function(Tday, Tnight, T_base, t_start, GDD_trs, k){
    n <- length(Tavg)

    GDD_day   <- pmax(T_base - Tday[t_start:n], 0) %>% cumsum()
    GDD_night <- pmax(T_base - Tnight[t_start:n], 0) %>% cumsum()
    GDD <- GDD_day*k + GDD_night*(1-k)

    EOS <- which(GDD >= GDD_trs)[1] + t_start - 1
    return(EOS)
}

#' preseason
#' 
#' @param EOS ending of season (day of year)
#' 
#' @examples
#' info_date <- data.table(date = dates, year = year(dates), yday = yday(dates))
#' @export
preseason <- function(info_date, Tmin, Tmax, Prec, Srad, EOS){
    # I_eos <- yday(EOS)
    dates

    df <- data.table(Tmin, Tmax, Prec, Srad) %>% cbind(info_date)

    d_pcor <- foreach(i = 1:6) %do% {
        nday = i*15
        I_start <- I_eos - nday + 1
        d <- df[yday <= I_eos & yday >= I_start, .(Tmin, Tmax, Prec, Srad), .(year)][, -1]
        ppcor(d)$estimate
    }
    # judge nday
}