#' pls_show
#' 
#' VIP, MC, attributable change, relative contribution
#' 
#' @import ggplot2
#' @export
pls_show <- function(pls_obj, nyear = 34, hjust = 2, vjust = -2, base_size = 16) 
{
    # Helvetica, Whitney Book
    library(gridExtra)
    library(data.table)
    theme_set( theme_bw(base_size = base_size) + #, base_family = "whit"
        theme(axis.text = element_text(size = base_size), 
            axis.title = element_text(size = base_size + 1, face = "bold")))

    fontface = "bold"
    fontsize_statistic = 5
    FUN_lab = label_median
    
    ngrid <- nrow(pls_obj$VIP)

    add_fig_No <- function(g, label = "(a)"){
        g + geom_text(data = data.table(x = Inf, y = Inf, label = label), 
                aes(x, y, label=label, fill=NULL), 
                hjust = hjust, vjust = vjust, size = 6, fontface = fontface) + 
            theme(legend.position = "none", panel.grid = element_blank()) 
    }

    add_boxplot <- function(p){
        # stat_boxplot(geom ='errorbar', width = 0.5) +
        # geom_boxplot2(width = 0.75) +
        p + stat_summary(fun.data = box_qtl, geom = "errorbar", width = 0.6) +
            geom_boxplot2(outlier.size = -1, coef = 0)
            # stat_summary(fun.y = mean, geom = "point")
    }
    
    melt_index <- function(mat) {
        cbind(row = 1:nrow(mat), mat) %>% data.table() %>% melt("row") 
    }
    
    d <- pls_obj$VIP %>% melt_index()
    
    d_rem <- d[!is.na(value), .N, .(variable)]
    d_rem[, label := sprintf("%s \n(%.1f%%)", variable, N/ngrid*100)]
    
    # d[is.na(value), value:=0]
    p_VIP <- {ggplot(d, aes(variable, value, fill = variable)) %>% add_boxplot()} + 
        geom_hline(yintercept = 1, color = "red", linetype = 1) + 
        geom_hline(yintercept = 0.8, color = "red", linetype = 2) + 
        # scale_x_discrete(breaks = d_rem$variable, labels = d_rem$label) +
        scale_y_continuous(breaks = c(0, 0.5, 0.8, 1, 1.5, 2)) + 
        labs(y = "VIP", x = NULL)
    p_VIP %<>% add_fig_No("(a)")

    color_zero = "black"
    # 2. std coef
    d <- pls_obj$std.coefs %>% melt_index()
    p_coef <- {ggplot(d, aes(variable, value, fill = variable)) %>% add_boxplot()} + 
        labs(y = "Standardized coefficients", x = NULL) + 
        geom_hline(yintercept = 0, color = color_zero, linetype = 2)
    p_coef %<>% add_fig_No("(b)")
    # geom_hline(yintercept = 1, color = "red", linetype = 2)
    
    # 3. delta change
    tidy_cont <- function(mat, is.fix = TRUE){
        mat %<>% as.matrix()
        # mat[, -1] %<>% {. /rowSums2(.) * mat[, 1]}
        colnames(mat)[1] <- "EOS"
        mat[abs(mat) >= 50] = 50 #NA_real_

        cbind(row = 1:nrow(mat), mat) %>% data.table() %>% melt("row") 
    }

    # 3. add attributable change
    d <- pls_obj$attribute_change %>% tidy_cont(TRUE)
    d[variable != "EOS", perc := abs(value)/sum(abs(value), na.rm = TRUE)*100, .(row)] # not y    
    # d[is.na(value), value:= 0]
    # browser()
    d[!is.na(value), .(mean = mean(value), median = median(value), sd = sd(value)), .(variable)] %>% 
        print()
    
    
    colors <- hue_pal()(5) %>% c("grey", .)
    p_delta <- {ggplot(d, aes(variable, value*nyear, fill = variable)) %>% add_boxplot()} + 
        # stat_summary(fun.data = FUN_lab, colour = "black", size = fontsize_statistic, geom = "text", vjust = -0.5) + 
        labs(y = "Attributable changes (days)", x = NULL) + 
        scale_fill_manual(values = colors) + 
        geom_hline(yintercept = 0, color = color_zero, linetype = 2)
    obs.mean <- d[variable == "EOS", .(value = mean(value, na.rm = TRUE)), .(variable)]
    p_delta <- p_delta + geom_point(data = obs.mean, aes(variable, value), size = 2, show.legend = FALSE)
    
    p_attribute <- {ggplot(d[variable != "EOS"], aes(variable, perc, fill = variable)) %>% add_boxplot()} + 
        stat_summary(fun.data = FUN_lab, colour = "black", size = fontsize_statistic, geom = "text", vjust = -0.5) +
        labs(y = "Relative contributions (%)", x = NULL)
        # scale_fill_manual(values = colors)
    
    # 考虑正负的贡献率
    # d[variable != "EOS", perc := value/sum(value, na.rm = TRUE)*100, .(row)] # not y    
    # p_attribute2 <- ggplot(d[variable != "EOS"], aes(variable, perc, fill = variable)) + 
    #     # stat_boxplot(geom ='errorbar', width = 0.5) +
    #     stat_summary(fun.data = box_qtl, geom = "errorbar", width = 0.6) +
    #     geom_boxplot2(outlier.size = -1, coef = 0) + 
    #     # geom_boxplot2(coef = 0, width = 0.8, notch = FALSE) 
    #     stat_summary(fun.data = FUN_lab, colour = "black", size = fontsize_statistic, 
    #         geom = "text", vjust = -0.5) +
    #     labs(y = "Relative contribution 2 (%)", x = NULL)
    #     # scale_fill_manual(values = colors)
# browser()

    p_delta %<>% add_fig_No("(c)")
    p_attribute %<>% add_fig_No("(d)")
    # p_attribute2 %<>% add_fig_No("(e)")
    # , p_attribute2
    g <- gridExtra::arrangeGrob(p_VIP, p_coef, p_delta, p_attribute, ncol = 2)
    g
}

    # stat_summary(fun.data = box_qtl, geom = "errorbar", width = 0.6) +
    # geom_boxplot2(coef = 0, width = 0.8, notch = FALSE) 
