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

    lst <- foreach(plsr = lst_plsr[names_sate], trend = lst_trend$MK[names_sate]) %do% {
        ans = attribute_change(plsr, trend, nyear)
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
      write_fig(p, gsub(".pdf$", ".tif", outfile), 11, 6.6)
    }
    # g = arrangeGrob(grobs = gs, nrow = 1)
    # write_fig(g, "a.pdf", 10, 7)
}
