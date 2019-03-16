
## ------------------------ GLOBAL VARIABLES -------------------------------
Cairo_Pdf <- function(p, file, width = 6, height = 6, times = 1){
    CairoPDF(file, width = width*times, height = height*times)
    print(p)
    dev.off()
}
  

################################################################################
Id_del <- c(16035, 18443, 20925, 21407, 21618, 21732, 21827)

vars_origin <- c("TRS2", "TRS5", "DER", "Zhang.spl.Senescence", "Zhang.spl.Dormancy", 
          "Zhang.beck.Senescence", "Zhang.beck.Dormancy",
          "Zhang.elmr.Senescence", "Zhang.elmr.Dormancy",
          "Gu.spl.DD", "Gu.spl.RD", 
          "Gu.beck.DD", "Gu.beck.RD",
          "Gu.elmr.DD", "Gu.elmr.RD")
vars <- c("TRS5", "TRS6", "DER", "Zhang.spl.Senescence", "Zhang.spl.Dormancy", 
          "Zhang.beck.Senescence", "Zhang.beck.Dormancy",
          "Zhang.elmr.Senescence", "Zhang.elmr.Dormancy",
          "Gu.spl.DD", "Gu.spl.RD", 
          "Gu.beck.DD", "Gu.beck.RD",
          "Gu.elmr.DD", "Gu.elmr.RD")

# vars_order <- vars_new[c(4:15, 1:3)]
# vars <- vars_new
oyg.colors <- colorRampPalette(c("green4", "yellow",  "red"))
# ggplot for plsr model coef theme initial

## ------------------------------- GLOBAL FUNCTIONS --------------------------
#  x: data.table class
filter_rational.dt <- function(x){
  x[x <= 0 | x > 366] <- NA
  x_list <- lapply(method.group, function(i) x[, i, with = FALSE])
  
  ## can adapt data.table class
  res <- lapply(x_list, function(x_df){
    Id_na <- apply(x_df, 1, order) %>% apply(2, function(x) {
      x_unique <- unique(diff(x))
      (length(x_unique) != 1) || (x_unique != 1)#should be attention that `|` different from `||`
    }) %>% which
    x_df[Id_na, ] <- NA#if Id_na is NULL have no any negtive effects
    x_df#
  })#quickly return, x_list.trim <-
  do.call(cbind, setNames(res, NULL))#return data.frame
}
filter_rational <- function(x){
  x[x <= 0 | x > 366] <- NA
  x_list <- lapply(method.group, function(i) x[, i])
  
  ## can adapt data.table class
  res <- lapply(x_list, function(x_df){
    Id_na <- apply(x_df, 1, order) %>% apply(2, function(x) {
      x_unique <- unique(diff(x))
      (length(x_unique) != 1) || (x_unique != 1)#should be attention that `|` different from `||`
    }) %>% which
    x_df[Id_na, ] <- NA#if Id_na is NULL have no any negtive effects
    x_df#
  })#quickly return, x_list.trim <-
  do.call(cbind, setNames(res, NULL))#return data.frame
}

## ----------------------- PLOT FUNCTIONS --------------------------
# change factor levels for plot
changeLevels <- function(x, at){
  result <- data.frame(x)
  for (i in 1:ncol(x))
    result[, i] <- cut(x[, i], breaks = at, include.lowest = T)
  result#quickly return
}

