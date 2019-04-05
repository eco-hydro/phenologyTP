#' pls_show
#' 
#' VIP, MC, attributable change, relative contribution
#' 
#' @import ggplot2
#' @export
pls_show <- function(pls_obj, nyear = 34, hjust = 2, vjust = 2.5, base_size = 14) 
{
    library(gridExtra)
    library(data.table)
    theme_set( theme_bw(base_size = base_size) + 
        theme(axis.text = element_text(size = base_size)))

    fontface = "bold"
    fontsize_statistic = 5
    FUN_lab = label_median

    add_fig_No <- function(g, label = "(a)"){
        g + geom_text(data = data.table(x = Inf, y = Inf, label = label), 
                aes(x, y, label=label, fill=NULL), 
                hjust = hjust, vjust = vjust, size = 6, fontface = fontface) + 
            theme(legend.position = "none", panel.grid = element_blank()) 
    }

    ngrid <- nrow(pls_obj$VIP)
    # 1. VIP value
    d <- pls_obj$VIP %>% cbind(row = 1:ngrid, .) %>% data.table() %>% melt("row") 
    
    d_rem <- d[!is.na(value), .N, .(variable)]
    d_rem[, label := sprintf("%s \n(%.1f%%)", variable, N/ngrid*100)]
    
    # d[is.na(value), value:=0]
    p_VIP <- ggplot(d, aes(variable, value, fill = variable)) + 
        stat_boxplot(geom ='errorbar', width = 0.5) +
        geom_boxplot2(width = 0.75) +
        geom_hline(yintercept = 1, color = "red", linetype = 1) + 
        geom_hline(yintercept = 0.8, color = "red", linetype = 2) + 
        # scale_x_discrete(breaks = d_rem$variable, labels = d_rem$label) +
        scale_y_continuous(breaks = c(0, 0.5, 0.8, 1, 1.5, 2)) + 
        labs(y = "VIP", x = NULL)
    p_VIP %<>% add_fig_No("(a)")

    color_zero = "black"
    # 2. std coef
    d <- pls_obj$std.coefs %>% cbind(row = 1:ngrid, .) %>% data.table() %>% melt("row") 
    p_coef <- ggplot(d, aes(variable, value, fill = variable)) + 
        stat_boxplot(geom ='errorbar', width = 0.5) +
        geom_boxplot2() + 
        labs(y = "Standardized model coefficients", x = NULL) + 
        geom_hline(yintercept = 0, color = color_zero, linetype = 2)
    p_coef %<>% add_fig_No("(b)")
    # geom_hline(yintercept = 1, color = "red", linetype = 2)
    
    # 3. delta change
    d <- pls_obj$attribute_change %>% cbind(row = 1:ngrid, .) %>% data.table() %>% melt("row") 
    # add attributable change
    # d[is.na(value), value:= 0]
    d[variable != "EOS", perc := abs(value)/sum(abs(value), na.rm = TRUE)*100, .(row)] # not y    
    colors <- hue_pal()(5) %>% c("grey", .)

    p_delta <- ggplot(d, aes(variable, value*nyear, fill = variable)) + 
        stat_boxplot(geom ='errorbar', width = 0.5) +
        geom_boxplot2(outlier.size = -1) + 
        # stat_summary(fun.data = FUN_lab, colour = "black", size = fontsize_statistic, geom = "text", vjust = -0.5) + 
        labs(y = "Attributable changes (days)", x = NULL) + 
        scale_fill_manual(values = colors) + 
        geom_hline(yintercept = 0, color = color_zero, linetype = 2)

    # browser()
    p_attribute <- ggplot(d[variable != "EOS"], aes(variable, perc, fill = variable)) + 
        stat_boxplot(geom ='errorbar', width = 0.5) +
        geom_boxplot2(outlier.size = -1) + 
        stat_summary(fun.data = FUN_lab, colour = "black", size = fontsize_statistic, geom = "text", vjust = -0.5) +
        labs(y = "Relative contribution to EOS change (%)", x = NULL)
        # scale_fill_manual(values = colors)
        
    p_delta %<>% add_fig_No("(c)")
    p_attribute %<>% add_fig_No("(d)")
    
    g <- gridExtra::arrangeGrob(p_VIP, p_coef, p_delta, p_attribute, nrow = 2)
    g
}
