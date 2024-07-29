## 5. Mixed effect modelling
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("lmerTest", "velociraptr", "sjPlot", "ggplot2", "velociraptr", "dplyr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(lmerTest)
library(velociraptr)
library(sjPlot)
library(ggplot2)
library(velociraptr)
library(dplyr)

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
                    "stages_s100")

output.strings <- c("stages_g200_mlm",
                   "stages_g100_mlm",
                   "stages_s200_mlm",
                   "stages_s100_mlm")

## Set working directory
setwd("~/R_packages/bivbrach")
home <- getwd()

## Set other parameters directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "~/R_packages/bivbrach/data"
source("functions/mass.mlm.R")

## mass.mlm arguments
#m = 1
#input.dir = input.dir
#input.pre = input.strings[m]
#output.dir = output.dir
#output.pre = output.strings[m]
#vars = vars
#vars.values = vars.values

## Run for each input
for(m in 1:length(input.strings)){
  mass.mlm(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
           output.pre = output.strings[m], vars = vars, vars.values = vars.values)
}

#### Plotting main results ####
## Define plot title strings
input.names <- c("sQ1_r1", "sQ2_r1", "sQ3_r1", "sQ4_r1",
                 "sQ1_r2", "sQ2_r2", "sQ3_r2", "sQ4_r2",
                 "sQ1_r3", "sQ2_r3", "sQ3_r3", "sQ4_r3")
plot.names <- c("2 sites, 200km radius", "3 sites, 200km radius", "4 sites, 200km radius", "5 sites, 200km radius",
                "2 sites, 500km radius", "3 sites, 500km radius", "4 sites, 500km radius", "5 sites, 500km radius",
                "2 sites, 1000km radius", "3 sites, 1000km radius", "4 sites, 1000km radius", "5 sites, 1000km radius")
title.strings <- cbind(input.names,plot.names)

## Download period colour palettes
periods <- downloadTime("international periods")
periods <- periods[order(periods$b_age, decreasing=TRUE), ]
periods <- periods[periods$b_age <= periods[which(rownames(periods) == "Cambrian"),"b_age"],c(2,4,5,8)]
periods[,"shape"] <- c(rep(16, 6), rep(15, 3), rep(17, 3))

## Download era shape palette
eras <- downloadTime("international eras")[1:3,]
eras <- eras[order(eras$b_age, decreasing = TRUE),c(2,4,5)]
eras[,"shape"] <- c(16,15,17)

## define input strings
input.strings <- c("stages_g200",
                   "stages_g100",
                   "stages_s200",
                   "stages_s100")

## Function
## Assemble plot title prefix
mass.SJplot <- function(input.string, argument.strings, model.input.dir, rich.input.dir, output.dir, times.col, period.scale, era.scale, xy){
  if(grepl("_g",input.string)){
    taxon <- "Genera,"
  } else {
    taxon <- "Species,"
  }
  if(grepl("200",input.string)){
    gCells <- "200km grid cells,"
  } else {
    gCells <- "100km grid cells,"
  }
  data.string <- paste(taxon, gCells)
  ## Read in model
  models <- readRDS(paste0(model.input.dir,"/",input.string,"_mlm_models.Rds"))
  for(i in 1:length(models)){
    ## Read in richness data
    richness <- read.csv(paste0(rich.input.dir, "/", input.string, "_", names(models)[i], ".csv"))
    ## Use period.scale to assign period information
    period <- c()
    for(t in 1:nrow(period.scale)){
      count <- length(which(between(richness[,times.col], left = period.scale[,"t_age"][t], right = period.scale[,"b_age"][t])))
      if(count > 0){
        period <- c(period, rep(period.scale[,"name"][t], count))
      }
    }
    era <- c()
    for(t in 1:nrow(era.scale)){
      count <- length(which(between(richness[,times.col], left = era.scale[,"t_age"][t], right = era.scale[,"b_age"][t])))
      if(count > 0){
        era <- c(era, rep(era.scale[,"name"][t], count))
      }
    }
    richness <- cbind(richness, period, era)
    ## Get colour vector
    point.col <- period.scale[,"color"]
    names(point.col) <- period.scale[,"name"]
    ## Get shape vector
    point.shape <- era.scale[,"shape"]
    names(point.shape) <- era.scale[,"name"]
    ## Get shape vector for data
    era.legend <- period.scale[,"shape"]
    era.legend <- era.legend[period.scale[,"name"] %in% unique(richness[,"period"])]
    ## Get plot title
    plot.title <- paste(data.string, argument.strings[which(argument.strings[,1] %in% names(models)[i]),2])
    ## define plot data frame
    line.df <- get_model_data(models[[i]], type = "pred", terms = xy[1])
    ## basic scatter plot
    scatter <- ggplot() +
      xlab("Bivalve richness") +
      ylab("Brachiopod richness") +
      labs(color = "Period") +
      ggtitle(plot.title) +
      geom_point(data = richness, aes(x = Bivalvia, y = Brachiopoda, color = period, shape = era)) +
      scale_color_manual(breaks = unique(richness[,"period"]), values = point.col) +
      scale_shape_manual(values = point.shape) +
      geom_line(data = line.df, aes(x = x, y = predicted)) +
      geom_ribbon(data = line.df, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1)+
      scale_x_continuous(expand = c(0,1)) +
      scale_y_continuous(expand = c(0,1)) +
      geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
      geom_vline(xintercept = 0, colour = "grey", linetype = "dashed") +
      guides(shape = "none",
             color = guide_legend(override.aes = list(shape = era.legend))) +
      theme(text = element_text(family = "Helvetica"),,
            title = element_text(size = 12),
            axis.text = element_text(size = 11),
            axis.title = element_text(size = 12),
            legend.title = element_text(size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "bottom",
            legend.text = element_text(size = 10),
            legend.key.size = unit(10,"point"))
    scatter
    ## plot
    pdf(file = paste0(output.dir, "/", plot.title, ".pdf"))
    print(scatter)
    dev.off()
  }
}

## Run function for all
model.input.dir <- "~/R_packages/bivbrach/data"
rich.input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "~/R_packages/bivbrach/figures/sjPlot"

for(r in 1:length(input.strings)){
 mass.SJplot(input.strings[r], title.strings, model.input.dir, rich.input.dir, output.dir, times.col = "times", period.scale = periods, era.scale = eras, xy = c("Bivalvia", "Brachiopoda"))
}

#### Sensitivity testing - taking median richness for each radially constrained region and dropping random effect ####
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- c("site_quota", "radius")

## input strings
## Set output strings
input.strings <- c("stages_g200",
                   "stages_g100",
                   "stages_s200",
                   "stages_s100")

output.strings.med.diff <- c("stages_g200_mlm_med_diff",
                    "stages_g100_mlm_med_diff",
                    "stages_s200_mlm_med_diff",
                    "stages_s100_mlm_med_diff")

output.strings.med.brach <- c("stages_g200_mlm_med_brach",
                             "stages_g100_mlm_med_brach",
                             "stages_s200_mlm_med_brach",
                             "stages_s100_mlm_med_brach")

output.strings.med.biv <- c("stages_g200_mlm_med_biv",
                             "stages_g100_mlm_med_biv",
                             "stages_s200_mlm_med_biv",
                             "stages_s100_mlm_med_biv")

output.strings.min.diff <- c("stages_g200_mlm_min_diff",
                             "stages_g100_mlm_min_diff",
                             "stages_s200_mlm_min_diff",
                             "stages_s100_mlm_min_diff")

## Set other parameters directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "~/R_packages/bivbrach/data"
source("functions/mass.mlm.on.summary.R")

#m = 1
#input.dir = input.dir
#input.pre = input.strings[m]
#output.dir = output.dir
#output.pre = output.strings.med.diff[m]
#vars = vars
#vars.values = vars.values
#mode = "median"
#unit = "diff"
#data.columns = c("Brachiopoda", "Bivalvia")

## Run for each input under each setting
for(m in 1:length(input.strings)){
  mass.mlm.on.summary(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
           output.pre = output.strings.med.diff[m], vars = vars, vars.values = vars.values, mode = "median", unit = "diff", data.columns = c("Brachiopoda", "Bivalvia"))
}

for(m in 1:length(input.strings)){
  mass.mlm.on.summary(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
                  output.pre = output.strings.med.brach[m], vars = vars, vars.values = vars.values, mode = "median", unit = "Brachiopoda", data.columns = c("Brachiopoda", "Bivalvia"))
}

for(m in 1:length(input.strings)){
  mass.mlm.on.summary(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
                  output.pre = output.strings.med.biv[m], vars = vars, vars.values = vars.values, mode = "median", unit = "Bivalvia", data.columns = c("Brachiopoda", "Bivalvia"))
}

for(m in 1:length(input.strings)){
  mass.mlm.on.summary(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
                  output.pre = output.strings.min.diff[m], vars = vars, vars.values = vars.values, mode = "min", unit = "diff", data.columns = c("Brachiopoda", "Bivalvia"))
}

#### Sensitivity testing - breaking up time periods ####
radii <- as.integer(c(200000, 500000, 1000000))
siteQuotas <- c(2, 3, 4, 5)
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)))
vars.values <- list(siteQuotas, radii)
names(vars.values) <- c("site_quota", "radius")

## input strings
## Set output strings
input.strings <- c("stages_g200",
                   "stages_g100",
                   "stages_s200",
                   "stages_s100")

output.strings <- c("stages_g200_mlm",
                    "stages_g100_mlm",
                    "stages_s200_mlm",
                    "stages_s100_mlm")

## Get stage data with times used (columns t_round and b_round)
stages <- downloadTime('international ages')
stages$name <- row.names(stages)

## Re-order stages by age, oldest first
stages <- stages[order(stages$b_age, decreasing=TRUE), ]

## Create columns for new rounded ages
stages$b_round <- stages$t_round <- 0

## Read in function for defining rounded stage ages
source("functions/round.age.R")

## Define groupings for rounding
groupings <- list(u10 <- which(stages$b_age < 10),
                  u150 <- which(stages$b_age < 150 & stages$b_age > 10),
                  old <- which(stages$b_age > 150))

## Round ages to different digits depending on age classification
for (group in 1:length(groupings)){
  bins <- groupings[[group]]
  digits <- c(2, 1, 0)[group]

  # round down younger boundary (terminus) and up older boundary (beginning) per stage
  for (i in bins){
    b <- stages$b_age[i]
    t <- stages$b_age[i+1]
    stages$b_round[i] <- round.age(b, digits=digits, round_up=TRUE)
    stages$t_round[i] <- round.age(t, digits=digits, round_up=FALSE)
  }
}

## Get time bins
era.cutoffs <- matrix(c(stages["Fortunian","b_round"],stages["Induan","b_round"],stages["Danian","b_round"],stages["Induan","b_round"],stages["Maastrichtian","t_round"],0), ncol = 2, nrow = 3)
colnames(era.cutoffs) <- c("bottom","top")
rownames(era.cutoffs) <- c("Paleozoic", "Mesozoic", "Cenozoic")

PTME.cutoffs <- matrix(c(stages["Fortunian","b_round"],stages["Induan","b_round"],stages["Induan","b_round"],0), ncol = 2, nrow = 2)
colnames(PTME.cutoffs) <- c("bottom","top")
rownames(PTME.cutoffs) <- c("Pre-PTME", "Post-PTME")

## Set other parameters directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "~/R_packages/bivbrach/data"
source("functions/mass.mlm.intervals.R")

#m = 1
#input.dir = input.dir
#input.pre = input.strings[m]
#output.dir = output.dir
#output.pre = output.strings[m]
#vars = vars
#vars.values = vars.values
#time.cutoffs <- era.cutoffs

## Run for each input
for(m in 1:length(input.strings)){
  mass.mlm.intervals(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
           output.pre = output.strings[m], vars = vars, vars.values = vars.values, time.cutoffs = era.cutoffs)
}

for(m in 1:length(input.strings)){
  mass.mlm.intervals(input.dir = input.dir, input.pre = input.strings[m], output.dir = output.dir,
                     output.pre = output.strings[m], vars = vars, vars.values = vars.values, time.cutoffs = PTME.cutoffs)
}

