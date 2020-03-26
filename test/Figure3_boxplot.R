source("test/main_pkgs.R")

load(file_plsr) 
load(file_trend)

load("data/00basement_TP.rda") 
load(file_pheno_010)
load(file_preseason)
ngrid = length(gridclip2_10)

melt_list2 <- function(res){
  melt_tree(res, c("type_trend", "type_source")) %>% 
    melt(c("type_trend", "type_source", "I"))  
}

{
  lst_trend = list(MK = l_mk.main, LM = l_lm.main)
  type_sources = names(lst_trend[[1]])
  
  years = c(16, 16, 14)
  # update attributable change
  res <- foreach(l_trend = lst_trend) %do% {
      foreach(obj = lst_plsr[type_sources], trend = l_trend, nyear = years) %do% {
          ans = attribute_change(obj, trend, nyear)
          x <- ans[, -1]
          x[abs(x) > 50] = 50
          cbind(ans[, 1], x)
      }
  }
  res.RC = map_depth(res, 2, function(d){
    d = d[, -(1:2)] %>% abs()
    sum = as.matrix(d) %>% rowSums2()
    perc = d/sum*100  
    cbind(I = 1:ngrid, perc)
  })
  # dorm
  res.dorm = map_depth(res, 2, function(d){
    d = d[, -(1:2)] %>% abs()
    dorm = apply(d, 1, which.max) %>% map_int(first)
    # browser()
    cbind(I = 1:ngrid, dorm) %>% as.data.table()
  })
  
  varnames = colnames(res[[1]][[1]])[-(1:2)]
  df    <- melt_list2(res)
  df.RC <- melt_list2(res.RC)
  df.dorm <- melt_tree(res.dorm, c("type_trend", "type_source"))
  df.dorm$dorm %<>% factor(labels = varnames)
}


## FIGURE 3: boxplot
Figure3 = TRUE
if (Figure3) {
    ## 2000-2015
    names_sate = c("GIMMS", "MOD13C1", "SPOT")
    years = c(16, 16, 14)

    lst <- foreach(plsr = lst_plsr[names_sate], ans = res$MK[names_sate], nyear = years) %do% {
        obj <- plsr$SOS
        obj$attribute_change = ans[, -1]
        d = ans[, -(1:2)] %>% abs()
        
        sum <- rowSums2(as.matrix(d))
        obj$RC = d/sum*100
        
        varnames = c("VIP", "std.coefs", "attribute_change", "RC")
        obj[varnames] #%>% map(as.data.table)
        # g1 <- pls_show(obj, nyear, hjust, vjust)
    }
    
    type = "SOS"
    gs = foreach(obj = lst, varname = names(lst), i = icount()) %do% {
      hjust <- switch(i, 2, 2, 3, 3)
      vjust = switch(i, 1.5, 1.5, 2.5, 2.5)
      
      outfile <- glue("Figures/Figure4_PLSR_{varname}_{type}.pdf")
      p <- pls_show(obj, nyear = 1, hjust, vjust)
      write_fig(p, gsub(".pdf$", ".pdf", outfile), 11, 6.6)
    }
    # g = arrangeGrob(grobs = gs, nrow = 1)
    # write_fig(g, "a.pdf", 10, 7)
}

# d <- map(lst, ~.$SOS$VIP %>%  data.table()) %>% melt_list("sate")
# d1 = d[, .N, .(sate)]
# d2 = d[SOS <= 0.8, .N, .(sate)]

# variable       mean      median       sd
# 1:      EOS -1.7243302 -2.73437500 8.781122
# 2:     Tmin -0.7336492 -0.14144217 8.613832
# 3:     Tmax  0.2281666  0.01207451 7.530536
# 4:     Prec -0.6018343 -0.15758746 7.978137
# 5:     Srad -0.3986154 -0.14670051 8.530643
# 6:      SOS -0.2442552 -0.05883240 7.568426
# variable         mean       median       sd
# 1:      EOS  1.425511694 -0.053846154 8.323376
# 2:     Tmin  0.636195314  0.131498933 8.123788
# 3:     Tmax  0.009441344 -0.002674334 7.615800
# 4:     Prec -0.079978067 -0.033008740 7.614269
# 5:     Srad  0.101953554 -0.020010268 7.680559
# 6:      SOS  0.667179395  0.026137275 7.056927
# variable        mean      median        sd
# 1:      EOS  2.13269994 -0.81262530 12.972117
# 2:     Tmin  1.92627289  0.18136346 11.849286
# 3:     Tmax  0.03980446 -0.01414608  9.373866
# 4:     Prec  0.13467908  0.01075259  9.035891
# 5:     Srad  0.28777806  0.03014576  9.932005
# 6:      SOS -0.80953285 -0.14468120  8.334032

