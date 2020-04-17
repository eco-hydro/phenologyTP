#' @import magrittr
IGBP006_names <- c(
    "UNC", "ENF", "EBF", "DNF", "DBF", "MF", "CSH",
    "OSH", "WSA", "SAV", "GRA", "WET", "CRO",
    "URB", "CNV", "SNO", "BSV", "water"
) %>% set_names(., .)
IGBP006_codes <- IGBP006_names %>% set_names(seq_along(.), .)

# note that: UNC and BSV not included
LCs <- c(
    "Others", "Forest", "Forest", "Forest", "Forest", "Forest", "Shrubland",
    "Shrubland", "Shrubland", "Shrubland", "Grassland", "Water", "Cropland",
    "Others", "Cropland", "Water", "Others", "Water"
)
# Others: UNC, URB, BAV
# Water: Water, SNO, WET

LCs_types = c("Forest", "Shrubland", "Grassland", "Cropland", "Water", "Others")
LCs_types_label = LCs_types %>% sprintf("(%s) %s", letters[seq_along(.)], .)

I_lc = seq_along(LCs)[-c(1, 17)]
years <- 2001:2018

info_LC = cbind(IGBP006_names, LCs) %>% data.table::data.table()

# visualization
#' @import ggplot2
fontsize = 13
mytheme_grey <- theme_grey(base_size = fontsize) # , base_family = "Times"
theme_set(mytheme_grey)
