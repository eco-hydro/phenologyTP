## 连续序列标记
ContinueTag <- function(X){
    Nind <- length(X)
    if (Nind == 0) return(0) #如果不存在连续干旱或湿润则返回空值
    if (Nind == 1) return(1) #如果仅有一天干旱或湿润

    Tag <- numeric(Nind)   #标记第几段连续序列
    nEvent = 1;Tag[1] <- 1 #第几段连续序列

    for (i in 1:(Nind-1)){
    if (X[i+1]!=X[i]+1) nEvent<-nEvent+1
        Tag[i+1] <- nEvent
    }
    Tag##quickly return
}

# 1 variables (excluding dimension variables):
#     short temp[lon,lat,time]   
# long_name: Near surface air temperature
# units: K
# add_offset: 273.149993896484
# scale_factor: 0.00999999977648258
# _FillValue: -32767
# missing_value: -32767
# PhenoThermal(x, T_SOS = 0, T_EOS = 0, nday_SOS = 5, nday_EOS = 5)
#' @export
PhenoThermal <- function(x, T_SOS = 0, T_EOS = 0, nday_SOS = 5, nday_EOS = nday_SOS, IsPlot = TRUE){
    if (all(is.na(x))) {
        return(c(SOS = NA, EOS = NA, LOS = NA))
    }
    SOS = NA
    EOS = NA
    
    n <- length(x)
    n_half <- floor(n/2)

    # starting of growing season
    I    <- which(x >= T_SOS)
    if (length(I) > 0) {
        grp  <- cumsum(c(1, diff(I) != 1 ))
        
        info <- data.table(I, grp)[I <= n_half, ]
        
        I_grp <- info[, .N, .(grp)][N >= nday_SOS, ]$grp[1]
        SOS   <- info[grp == I_grp, I][1] + nday_SOS - 1 # growing season begin
    }
    
    # ending of growing season
    I    <- which(x <= T_EOS)
    if (length(I) > 0) {
        grp  <- cumsum(c(1, diff(I) != 1 ))
        info <- data.table(I, grp)[I > n_half, ]
        
        I_grp <- info[, .N, .(grp)][N >= nday_EOS, ]$grp[1]
        EOS   <- info[grp == I_grp, I][1] + nday_EOS - 1 # growing season begin
    }
    
    res <- c(SOS = SOS, EOS = EOS, LOS = EOS - SOS + 1)

    if (IsPlot) {
        plot(x, type = "b")
        grid()

        abline(h = T_SOS, col = "blue")
        abline(h = T_EOS, col = "red")
        abline(v = SOS, col="blue")
        abline(v = EOS, col="red")

        s <- wSG(x, nptperyear = 365, frame = 15); lines(s$zs$ziter2, col = "red", lwd = 2)
    }
    return(res)
}


## 5 Growing season length: Annual (1st Jan to 31st Dec in Northern Hemisphere (NH), 1st July to 30th 
#  June in Southern Hemisphere (SH)) count between first span of at least 6 days with daily mean 
#  temperature TG>5oC and first span after July 1st (Jan 1st in SH) of 6 days with TG<5oC. 
#  1-6月的第一次至少连续6日平均气温高于定义温度至7-12月第一次至少连续6日平均气温高于定义温度的持续天数
#  生长季指数要求数据为整年，非整年的部分自动截去
#  未记录生长季的起始时间
clim.GSL <- function(Taver){
    if (length(Taver) < 365) {
        warning("生长季计算只能按照年尺度")
        return(NA)
    }
    Nmid <- floor(length(Taver)/2); N <- length(Taver)
    ## 假定生长季Taver数据是整年输入，则生长季开始时间在1:(n/2)段，结束点在[(n/2)+1]:n段，n表示数据长度
    Id_begin <- which(Taver > 5)
    Tag <- ContinueTag(Id_begin)
    segment.length <- sapply(1:Tag[length(Tag)], function(i) length(which(Tag == i)))
    TagId <- which(segment.length >= 6)[1]            ##如果查找不到则返回空值
    point.begin <- Id_begin[which(Tag == TagId)[1]]   ##生长季开始点需要在7月之前,如果未查找到则为空值，空值报错

    Id_end <- which(Taver[(Nmid+1):N] < 5) + Nmid
    Tag <- ContinueTag(Id_end)
    segment.length <- sapply(1:Tag[length(Tag)], function(i) length(which(Tag == i)))
    TagId <- which(segment.length >= 6); TagId <- TagId[1]            ##如果查找不到则返回空值
    point.end <- Id_end[which(Tag == TagId)]; point.end <- point.end[1]##生长季开始点需要在7月之前,如果未查找到则为空值，空值报错
    if ((length(point.begin)==0) | (length(point.end)==0 ))  return(NA)##数据过短
    if (is.na(point.begin) | is.na(point.end))  return(NA)##数据过短.;

    if (point.begin >= 183) point.begin <- NA
    if (point.end < 183) end.point <- NA
    if (is.na(point.begin) | is.na(point.end)) warning("生长季的起始时间不在规定范围内，请核对数据！")
    point.end - point.begin##return gsl,如果起始点不在1-6月，结束点不在7-12月则返回空值
}


