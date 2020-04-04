
label_value <- function (labels, multi_line = TRUE)
{
    labels <- lapply(labels, as.character)
    # labels[[1]] <- labs
    # browser()
    if (multi_line) {
        labels
    }
    else {
        collapse_labels_lines(labels)
    }
}

{
    # library(rnaturalearth)
    # library(rnaturalearthdata)
    # # large
    # world <- ne_countries(scale = "medium", returnclass = "sp")
    sp_layout <- list("sp.polygons", world, first = FALSE, lwd = 0.2) # , fill = "transparent"
    # class(world)
}


## load phenology_TP -----------------------------------------------------------
prj_TP               <- path.mnt("n:/Research/phenology/phenology_TP/")
file_pheno_012       <- paste0(prj_TP, "OUTPUT/phenology_TP_AVHRR_phenofit.rda")
# file_pheno_010       <- paste0(prj_TP, "OUTPUT/phenology_TP_AVHRR_phenofit_010deg.rda"
file_pheno_010       <- paste0(prj_TP, "OUTPUT/phenology_TP_AVHRR_phenofit_010deg.rda")
file_pheno_010_3s    <- paste0(prj_TP, "OUTPUT/phenology_TP_phenology_010deg_3s.rda")
file_pheno_010_3s_V2 <- paste0(prj_TP, "OUTPUT/phenology_TP_phenology_010deg_3s_V2.rda")

file_preseason <- paste0(prj_TP, "OUTPUT/TP_010deg_preseason2.rda")
file_plsr      <- paste0(prj_TP, "OUTPUT/TP_010deg_PLSR_SOS and Non-SOS_V2.rda")
file_plsr_mk   <- paste0(prj_TP, "OUTPUT/TP_010deg_PLSR_SOS and Non-SOS_(slope_mk).rda")

## -----------------------------------------------------------------------------
range = c(-180, 180, -60, 90)
grid  <- get_grid(range, 0.1, type = "vec")
grid5 <- get_grid(range, 0.5, type = "vec")

range_TP = c(73, 105, 25, 40)
grid_TP <- get_grid(range_TP, 0.1, type = "vec")
# get_grid(range, 0.1, type = "vec")

load("../phenology_TP/data/00basement_TP.rda")
l <- clip_raster(raster(grid), grid_TP)

labs <- c(
    expression(d[GPP] / d[LAI]),
    expression(d[Ec] / d[LAI]),
    expression(d[Es] / d[LAI]),
    expression(d[Ei] / d[LAI])
)

mete_variables <- c("Aridity", "Tair", "Prcp", "D")
lst_lims <- list(
    Aridity = c(0, 10),
    Tair = c(0, 30),
    Prcp = c(0, 1500),
    D = c(-1500, 1000)
)

# set probs
alphas <- 1 - c(0.95, 0.9, 0.8, 0.5)
d_prob <- map_dfr(alphas, ~ data.table(alpha = ., min = . / 2, max = 1 - . / 2)) %>%
    melt("alpha", value.name = "prob") %>%
    rbind(data.table(alpha = 1, variable = "mid", prob = 0.5))
probs <- sort(d_prob$prob)

levs <- unique(d_prob$alpha) # %>% rev()
nlev <- length(levs)
cols <- hue_pal()(nlev - 1) # %>% rev()
