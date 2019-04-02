# NA brk will return NA
check_turnpoint <- function(x, nyear = 34, min = 5) {
    brk <- pmin(x[1], nyear - min + 1) %>% 
        pmax(min, min)
    return(brk)
}

#' PieceWise Regression
#' 
#' @param y Numeric vector
#' @param year.origin first year of y
#' @param predict If true, returned predicted data of piecewise regression.
#' @param brk If provide, changing point detected will not applied.
#' 
#' @import segmented
#' @export
piecewise <- function(y, year.origin = 1, predict = FALSE, brk = NULL){
    n <- length(y)
    middle <- floor(n/2)
    pvalue <- NA
    
    if (!is.null(brk)) {
        if (length(unique(y)) > 3){#如果全部为0,则把该点数据剔除
            x <- seq_along(y)
            lm_fit <- lm(y ~x)
            # test beta2 significant
            dtest <- davies.test(lm_fit, ~x, k = 10)
            pvalue <- dtest$p.value#pvalue越小越好
            #如果dtest返回的有brk，则将此brk作为segmented的起始值
            brk_init <- tryCatch(floor(dtest$statistic[[1]]), error = function(e) middle)
            piece_fit <- segmented(lm_fit, seg.Z = ~x, brk = brk_init, 
                                 control = seg.control(stop.if.error = FALSE, n.boot = 0, it.max = 100))
        
            psi <- piece_fit$psi
            if ( !is.null(psi) && (length(psi) >= 1)){#判断是否存在突变点
                brk <- floor(psi[2]) # Initial, Est., St.Err
                #brk <- ifelse(is.na(brk), 16, brk)#断点不存在时处理
            }else{
                brk <- tryCatch(floor(dtest$statistic[[1]]), error = function(e) NA)
            }
        }

        brk %<>% check_turnpoint(n, min = 5)
    }    
    
    y_a  <- y[1:brk]
    y_b  <- y[(brk+1):length(x)]

    trend_a   <- mkTrend(y_a)
    trend_b   <- mkTrend(y_b)
    trend_all <- mkTrend(y)

    trend <- rbind(trend_a, trend_b, trend_all) %>% 
        data.table(period = c("before", "after", "whole"), .)
    ans <- list(trend = trend, turnpoint = data.frame(brk = brk+year.origin-1, pvalue = pvalue))
    
    if (predict) {
        ans$pred <- predict_mk(n, brk, trend_a, trend_b, year.origin)
    }
    ans
}

predict_mk <- function(n, brk, trend_a, trend_b, year.origin = 1){
    x_a = (1:brk)
    x_b = ((brk+1)):n
    y_a = trend_a['slp']*x_a + trend_a['intercept']
    y_b = trend_b['slp']*x_b + trend_b['intercept']

    d <- rbind(data.table(x = x_a , y = y_a, period = "before"), 
        data.table(x = x_b, y = y_b, period = "after"))

    d$x <- d$x + year.origin - 1
    d
}

