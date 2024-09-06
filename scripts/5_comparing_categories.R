## 5. Species richness of different categories
## Started by TJS on 04/09/2024

# Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("sjPlot")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(sjPlot)

## Load variable vectors - just looking at sites moving forward.
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- names(vars) <- c("site_quota", "radius")

## input strings
## Set output strings
input.strings <- c("stages_g200",
                   "stages_g100",
                   "stages_s200",
                   "stages_s100",
                   "stages_g200_epif",
                   "stages_g100_epif",
                   "stages_s200_epif",
                   "stages_s100_epif",
                   "stages_g200_inf",
                   "stages_g100_inf",
                   "stages_s200_inf",
                   "stages_s100_inf",
                   "stages_g200_sitesThenRefs",
                   "stages_g100_sitesThenRefs",
                   "stages_s200_sitesThenRefs",
                   "stages_s100_sitesThenRefs")

output.strings.env <- c("stages_g200_environment",
                   "stages_g100_environment",
                   "stages_s200_environment",
                   "stages_s100_environment",
                   "stages_g200_epif_environment",
                   "stages_g100_epif_environment",
                   "stages_s200_epif_environment",
                   "stages_s100_epif_environment",
                   "stages_g200_inf_environment",
                   "stages_g100_inf_environment",
                   "stages_s200_inf_environment",
                   "stages_s100_inf_environment",
                   "stages_g200_sitesThenRefs_environment",
                   "stages_g100_sitesThenRefs_environment",
                   "stages_s200_sitesThenRefs_environment",
                   "stages_s100_sitesThenRefs_environment")

output.strings.lith <- c("stages_g200_lithology",
                        "stages_g100_lithology",
                        "stages_s200_lithology",
                        "stages_s100_lithology",
                        "stages_g200_epif_lithology",
                        "stages_g100_epif_lithology",
                        "stages_s200_epif_lithology",
                        "stages_s100_epif_lithology",
                        "stages_g200_inf_lithology",
                        "stages_g100_inf_lithology",
                        "stages_s200_inf_lithology",
                        "stages_s100_inf_lithology",
                        "stages_g200_sitesThenRefs_lithology",
                        "stages_g100_sitesThenRefs_lithology",
                        "stages_s200_sitesThenRefs_lithology",
                        "stages_s100_sitesThenRefs_lithology")

output.strings.reef <- c("stages_g200_reefalState",
                        "stages_g100_reefalState",
                        "stages_s200_reefalState",
                        "stages_s100_reefalState",
                        "stages_g200_epif_reefalState",
                        "stages_g100_epif_reefalState",
                        "stages_s200_epif_reefalState",
                        "stages_s100_epif_reefalState",
                        "stages_g200_inf_reefalState",
                        "stages_g100_inf_reefalState",
                        "stages_s200_inf_reefalState",
                        "stages_s100_inf_reefalState",
                        "stages_g200_sitesThenRefs_reefalState",
                        "stages_g100_sitesThenRefs_reefalState",
                        "stages_s200_sitesThenRefs_reefalState",
                        "stages_s100_sitesThenRefs_reefalState")

output.strings.lat <- c("stages_g200_latitude",
                        "stages_g100_latitude",
                        "stages_s200_latitude",
                        "stages_s100_latitude",
                        "stages_g200_epif_latitude",
                        "stages_g100_epif_latitude",
                        "stages_s200_epif_latitude",
                        "stages_s100_epif_latitude",
                        "stages_g200_inf_latitude",
                        "stages_g100_inf_latitude",
                        "stages_s200_inf_latitude",
                        "stages_s100_inf_latitude",
                        "stages_g200_sitesThenRefs_latitude",
                        "stages_g100_sitesThenRefs_latitude",
                        "stages_s200_sitesThenRefs_latitude",
                        "stages_s100_sitesThenRefs_latitude")


## Start with one to begin with
## Set inout and output directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dirs <- c("~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/genera_200km_cells/base/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/genera_100km_cells/base/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/species_200km_cells/base/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/species_200km_cells/base/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/genera_200km_cells/epifaunal/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/genera_100km_cells/epifaunal/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/species_200km_cells/epifaunal/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/species_200km_cells/epifaunal/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/genera_200km_cells/infaunal/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/genera_100km_cells/infaunal/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/species_200km_cells/infaunal/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/species_200km_cells/infaunal/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/genera_200km_cells/sitesThenRefs/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/genera_100km_cells/sitesThenRefs/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/species_200km_cells/sitesThenRefs/",
                 "~/R_packages/bivbrach/figures/lmm/richness_boxplots_comparing_categories/species_200km_cells/sitesThenRefs/")


input.pre <- input.strings[1]
output.pre <- output.strings.env[1]
output.dir <- output.dirs[1]

combin <- expand.grid(vars)
varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".csv")
output.dir.mlm <- paste0(output.dir, "/", output.pre, "_models.Rds")
output.dir <- paste0(output.dir, "/", output.pre, ".csv")

i = 1
data <- suppressWarnings(tryCatch(read.csv(input.dirs[i], row.names = 1), error = function(e){}))

plot_grpfrq(data$Brachiopoda, var.grp = data$sampEnv, type = "box")
