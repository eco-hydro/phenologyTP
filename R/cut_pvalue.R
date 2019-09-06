#' @export
cut_pvalue <- function(pval, slp){
    brks = c(0.1, Inf) %>% c(-rev(.), 0, .)
    # (-Inf,-0.1] (-0.1,0] (0,0.1] (0.1, Inf]
    labels <- c("non-significant", "- (p<= 0.1)", "+ (p<= 0.1)", "non-significant")
    labels_fix <- c("- (p<= 0.1)", "non-significant", "+ (p<= 0.1)")
    
    # brks = c(0.05, 0.1, Inf) %>% c(-rev(.), 0, .)
    # # (-Inf,-0.1] (-0.1,-0.05] (-0.05,0] (0,0.05] (0.05,0.1] (0.1, Inf]
    # labels <- c("non-significant", "- (p<= 0.1)", "- (p<=0.05)", 
    #     "+ (p<=0.05)", "+ (p<= 0.1)", "non-significant")
    # labels_fix <- c("- (p<=0.05)", "- (p<= 0.1)", "non-significant",  
    #             "+ (p<= 0.1)", "+ (p<=0.05)")
    
    cut(pval*sign(slp), brks, labels) %>% factor(labels_fix)
}
