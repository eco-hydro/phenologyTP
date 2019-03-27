#' pls_show
#' 
#' @import ggplot2
#' @export
pls_show <- function(pls_obj, base_size = 14) 
{
    library(gridExtra)
    library(data.table)
    theme_set( theme_bw(base_size = base_size) + 
        theme(axis.text = element_text(size = base_size)))
    
    # 1. VIP value
    d <- pls_obj$VIP %>% cbind(row = 1:ngrid, .) %>% data.table() %>% melt("row") 
    
    d_rem <- d[!is.na(value), .N, .(variable)]
    d_rem[, label := sprintf("%s \n(%.1f%%)", variable, N/ngrid*100)]
    
    # d[is.na(value), value:=0]
    p_VIP <- ggplot(d, aes(variable, value, fill = variable)) + 
        stat_boxplot(geom ='errorbar', width = 0.5) +
        # geom_violin() + 
        geom_boxplot2(width = 0.75) +
        geom_hline(yintercept = 1, color = "red", linetype = 1) + 
        geom_hline(yintercept = 0.8, color = "red", linetype = 2) + 
        # scale_x_discrete(breaks = d_rem$variable, labels = d_rem$label) +
        scale_y_continuous(breaks = c(0, 0.5, 0.8, 1, 1.5, 2)) + 
        labs(y = "VIP", x = NULL) + 
        theme(legend.position = "none", 
            panel.grid = element_blank())
    
    # 2. std coef
    d <- pls_obj$std.coefs %>% cbind(row = 1:ngrid, .) %>% data.table() %>% melt("row") 
    p_coef <- ggplot(d, aes(variable, value, fill = variable)) + 
        stat_boxplot(geom ='errorbar', width = 0.5) +
        geom_boxplot2() + 
        labs(y = "Standardized model coefficients", x = NULL) + 
        theme(legend.position = "none", 
            panel.grid = element_blank())
        geom_hline(yintercept = 0, color = "red", linetype = 2) 
    # geom_hline(yintercept = 1, color = "red", linetype = 2)
    
    # 3. delta change
    d <- pls_obj$attribute_change %>% cbind(row = 1:ngrid, .) %>% data.table() %>% melt("row") 
    # add attributable change
    # d[is.na(value), value:= 0]
    d[variable != "EOS", perc := abs(value)/sum(abs(value), na.rm = TRUE)*100, .(row)] # not y
    
    p_delta <- ggplot(d, aes(variable, value*34, fill = variable)) + 
        stat_boxplot(geom ='errorbar', width = 0.5) +
        geom_boxplot2(outlier.size = -1) + 
        geom_hline(yintercept = 0, color = "red", linetype = 2) + 
        # geom_errorbar() + 
        labs(y = "Attributable changes (days)", x = NULL) + 
        theme(legend.position = "none", 
              panel.grid = element_blank())
    
    # browser()
    p_attribute <- boxplot2(d, "perc", "Relative contribution to EOS change (%)")

    g <- gridExtra::arrangeGrob(p_VIP, p_coef, p_delta, p_attribute, nrow = 2)
    g
}
