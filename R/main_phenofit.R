# source('R/mainfunc/main_phenofit.R', encoding = "utf-8")

tidy_rda <- function(file){
    load(file)
    lst <- lapply(pheno, function(xx) {
        out <- lapply(xx, function(p){
            if (length(p) == 1){
                rep(NA, 17)
            }else{
                as.numeric(unlist(p))
            }
        })
        if (sum(sapply(out, length)) != 68){
            NA
        }else{
            out
        }
    })
    return(lst)
}


#' pheno_valid
#' 
#' @param pheno_sat_grow A List, with the length of years, the elements of every
#' list are phenology metrics (This time was 17 matrix, TRS2, TRS5, TR6, DES, GU, ZHANG)
#' @param pheno_obs A dataframe,  at least have year and DOY column
#' 
#' global params:
#' pheno_obs.SOS, pheno_obs.EOS
pheno_valid <- function(pheno_sat.grow, pheno_obs.SOS, pheno_obs.EOS){
    #' @param pheno_sat A matrix, dims = [year, methods], same as pheno_sat.grow, 
    #                   But, one for spring or autumn phenology methods. 
    #' @param pheno_obs A dataframe,  at least have year and DOY column
    valid <- function(pheno_sat, pheno_obs){
        I     <- pheno_obs$year - 1981
        #column first priority, so it can directly subtract
        diffs <- pheno_sat[I, ] - pheno_obs$doy
        stats <- adply(pheno_sat[I, ], 2, GOF, Y_obs = pheno_obs$doy, .id = "var")
        # stats$n_obs <- nrow(pheno_obs)
        return(stats[, 1:4])  
    }    
    #satellite phenology: nrow = 34, 1982-2015
    pheno_sat.SOS <- sapply(pheno_sat.grow, `[`, i = I_SOS) %>% t %>% set_colnames(varnames[I_SOS])
    pheno_sat.EOS <- sapply(pheno_sat.grow, `[`, i = I_EOS) %>% t %>% set_colnames(varnames[I_EOS])
    
    list(SOS = valid(pheno_sat.SOS, pheno_obs.SOS),
         EOS = valid(pheno_sat.EOS, pheno_obs.EOS)) #QUICKLY RETURN
}

## AUTUMN PHENOLOGY
## SPRING PHENOLOGY
show_valid <- function(df, phase = c("SOS", "EOS")){
    phase <- phase[1]
    line_df <- data.frame(index = c("Bias", "Bias","MAE", "RMSE"), val = c(15, -15, 15, 15))
    theme_set(theme_grey(base_size = 16, base_family = "Arial"))
    subplot <- function(p){
        p + geom_boxplot() +
            facet_grid(index~curveM, scales = "free_y") + 
            # ggtitle(phase) + 
            xlab("Phenology metrics") + ylab("") + 
            geom_hline(aes(yintercept = val), line_df, color = "red", linetype=2, size = 0.6) + 
            geom_hline(yintercept = 30, color = "red", linetype = 1, size = 0.6) +
            # scale_y_continuous(limits = )
            theme(axis.text = element_text(face = "bold"),
                axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
                  axis.title.y = element_blank(),
                  strip.text = element_text(lineheight=0, margin = margin(0,0,0,0, "cm"), face = "bold"))  
    }
    #  1. Phenology metric & Curve fitting methods
    p1 <- ggplot(df[df$phase == phase, ], aes(var, value, color = phenoM)) %>%
        subplot
    #  2. Only phenology extraction methods were checked. 
    p2 <- ggplot(df[df$phase == phase, ], aes(var, value, color = var, group = var)) %>%
        subplot
    #  3. Only curve fitting methods were checked, i.e AG, Beck, ELMORE, SPLINE. 
    p3 <- ggplot(df[df$phase == phase, ], aes(var, value, color = phenoM, group = phenoM)) %>% 
        subplot
    
    Cairo_Pdf(p1, file = paste0(phase, "_p1.pdf"), 11.83, 6.39, times = 1.5) 
    Cairo_Pdf(p2, file = paste0(phase, "_p2.pdf"), 11.83, 6.39, times = 1.5)
}
