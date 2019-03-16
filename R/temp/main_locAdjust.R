# calculate landcover percentage for every grids of SPOT or MODIS 
#   using glc30 dataset
get_lc_Perc <- function(dsn, layer){
  # landcover value: 
  lc_id   = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  lc_name = c("Cultivated land", "Forest", "Grassland", "Shrubland", "Wetland", 
              "Water bodies", "Tundra", "Artificial Surfaces", "Bareland", 
              "Permannent snow and ice")
  
  vars = c("id", "gridcode", "Shape_Area")
  # get landcover percentage for each spot or modis grid
  # info <- ddply(x, .(id, gridcode), sum)
  # info <- ddply(x, .(id, gridcode), summarise, area = sum(Shape_Area))
  lc_Isect <- rgdal::readOGR(dsn, layer)@data[, vars]
  
  x <- as.data.table(lc_Isect) %>% 
    .[, .(area = sum(Shape_Area)), by = .(id, gridcode)] %>%
    dcast(id~gridcode, value.var = "area")
  
  # confirm id is correct
  df <- adply(as.matrix(x[, 2:9]), 1, function(x) x/sum(x, na.rm = T), .id = NULL)
  colnames(df) <- lc_name[c(1:6, 8, 9)]
  return(data.frame(id = x$id, df))
} 

## MODIS
# select grids according to landcover percentage nearest agro-meteorological station
#
#' select_lc.grids
#' 
#' @param x returned by previous step
#' x 'data.frame':  23 obs. of  14 variables:
#' $ stationId          : int  53723 53723 53723 53723 53723 53723 53723 53723 53723 53723 ...
#' $ nearId             : int  1 7 8 9 10 11 13 15 16 17 ...
#' $ idx                : num  1408306 1407785 1408827 1408308 1407783 ...
#' $ dists              : num  0.329 1.328 1.548 1.693 1.717 ...
#' $ id                 : int  623189 1545837 1608653 1645862 1878249 1969112 2229119 2288864 2316783 2329857 ...
#' $ Cultivated.land    : num  0.823 0.944 0.797 0.509 0.763 ...
#' $ Forest             : num  NA NA NA 0.007303 0.000954 ...
#' $ Grassland          : num  0.00573 NA NA 0.0659 0.00726 ...
#' $ Shrubland          : num  NA NA NA NA NA NA NA NA NA NA ...
#' $ Wetland            : num  NA NA NA NA NA NA NA NA NA NA ...
#' $ Water.bodies       : num  NA NA NA 0.035 NA ...
#' $ Artificial.Surfaces: num  0.1716 0.0557 0.2028 0.3832 0.2289 ...
#' $ Bareland           : num  NA NA NA NA NA ...
#' $ landcover          : chr  "草地,耕地" "草地,耕地" "草地,耕地" "草地,耕地" ..
select_lc.grids <- function(x){
  if (x$landcover[1] == "草地"){
    res <- subset(x, Grassland > 0.5) %>% 
      {.[with(., order(Grassland, decreasing = T)), ]}
    
    #if max cultivated land didn't exceed 0.5
    if (nrow(res) == 0) {
      res <- x[order(x$Grassland, decreasing = T)[1:3], ]
    }
    res$lc <- "草地"
  }else if(x$landcover[1] == "耕地"){
    res <- subset(x, Cultivated.land > 0.5) %>% 
      {.[with(., order(Cultivated.land, decreasing = T)), ]}
    
    #if max cultivated land didn't exceed 0.5
    if (nrow(res) == 0) {
      res <- x[order(x$Cultivated.land, decreasing = T)[1:3], ]
    }
    res$lc <- "耕地"
  }else{
    #if grassland and cultivate land mixed
    # 50936: grassland 0.27, 0.178, 0.163
    # 53723: grassland 0.89, 0.55, 27.2
    # hence select 3 grassland grids, and 6 cultivated land
    Id_grass <- with(x, id[order(Grassland, decreasing = T)[1:3]])
    Id_farm <- subset(x, Cultivated.land > 0.5) %>% 
      with(., id[order(Cultivated.land, decreasing = T)])
    if (length(Id_farm) > 6) Id_farm <- Id_farm[1:6]

    res <- x[match(c(Id_grass, Id_farm), x$id), ]
    res$lc <- "耕地"
    res$lc[match(Id_grass, res$id)] <- "草地"
  }
  if (nrow(res) > 9) res <- res[1:9, ]
  return(res)
}
