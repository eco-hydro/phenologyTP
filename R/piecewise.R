# NA brk will return NA
check_turnpoint <- function(x, nyear = 34, min = 5) {
    brk <- pmin(x[1], nyear - min + 1) %>% 
        pmax(min, min)
    return(brk)
}

#' PieceWise Regression
#' 
#' @param y
#' 
#' @import segmented
#' @export
piecewise <- function(y){
    n = length(y)
    brk <- NA
    pvalue <- NA
    
    if (length(unique(y)) > 3){#如果全部为0,则把该点数据剔除
        x <- seq_along(y)
        lm_fit <- lm(y ~x)
        # test beta2 significant
        dtest <- davies.test(lm_fit, ~x, k = 10)
        pvalue <- dtest$p.value#pvalue越小越好
        #如果dtest返回的有brk，则将此brk作为segmented的起始值
        brk_init <- tryCatch(floor(dtest$statistic[[1]]), error = function(e) 16)
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

    brk %<>% check_turnpoint()

    y_before <- y[1:brk]
    y_after  <- y[(brk+1):length(x)]

    trend <- rbind(mkTrend(y_before), mkTrend(y_after), mkTrend(y)) %>% 
        data.table(period = c("before", "after", "whole"), .)
    list(trend = trend, turnpoint = data.frame(brk = brk, pvalue = pvalue))
}
