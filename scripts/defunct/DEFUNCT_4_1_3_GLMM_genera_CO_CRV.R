## 4.1.3 GLMM - genera, original covariates, classical rarefaction (variable)
## Started by TJS on 27/06/2025

#### Startup ####
## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("lme4", "MuMIn", "sjmisc", "glmmTMB", "DHARMa", "performance", "car", "divDyn", "gstat", "raster", "terra", "xtable", "sjPlot", "ggplot2", "palaeoverse", "ggeffects", "stringr", "simr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(divDyn)
library(MuMIn)
library(sjmisc)
library(lme4)
library(car)
library(glmmTMB)
library(DHARMa)
library(performance)
library(gstat)
library(raster)
library(terra)
library(sjPlot)
library(ggplot2)
library(palaeoverse)
library(ggeffects)
library(stringr)
library(simr)
library(parallel)
library(cowplot)

## Load data
raw_data <- read.csv("data/analysis_data/genera_CO_CRV.csv", header = T, row.names = 1)

#### Prepping data ####
## Drop columns not being used in model
raw_data <- raw_data[,c(-1, -3)]

## Check numeric entries are numeric
nums <- c(2:9, 11)
for(i in nums){
  if(!is.numeric(raw_data[,i])){
    raw_data[,i] <- as.numeric(raw_data[,i])
  }
}

## And factors are factors
factors <- c(1,10)
for(i in factors){
  if(!is.numeric(raw_data[,i])){
    raw_data[,i] <- as.factor(raw_data[,i])
  }
}

## Relevel PTME factor
raw_data[,"PTME"] <- relevel(raw_data[,"PTME"], ref = "PrePTME")

## Rename to simplify models
colnames(raw_data) <- c("stage", "bivalve", "brachiopod", "long", "lat", "lith", "bath", "reef", "AbsLat", "PTME", "n")

## Some counts are decimal. Round these to discrete values to work with negative binomial
raw_data$bivalve <- round(raw_data$bivalve, digits = 0)
raw_data$brachiopod <- round(raw_data$brachiopod, digits = 0)

## Standardise predictors
raw_data <- std(raw_data, raw_data[,c(2, 6, 7, 8, 9, 11)])

## Drop non-standardized predictors
raw_data <- raw_data[,c(-2, -6, -7, -8, -9, -11)]

## Re-do names
colnames(raw_data) <- c("stage", "brachiopod", "long", "lat", "PTME", "bivalve", "lith", "bath", "reef", "AbsLat", "n")

#### Explore relationship between variables and brachiopod richness ####
plot(x = raw_data$bivalve, y = raw_data$brachiopod)
plot(x = raw_data$lith, y = raw_data$brachiopod)
plot(x = raw_data$bath, y = raw_data$brachiopod)
plot(x = raw_data$reef, y = raw_data$brachiopod)
plot(x = raw_data$AbsLat, y = raw_data$brachiopod)
plot(x = raw_data$n, y = raw_data$brachiopod)
## Pre/Post-PTME
prePTME <- raw_data[which(raw_data$PTME == "PrePTME"),"brachiopod"]
postPTME <- raw_data[which(raw_data$PTME == "PostPTME"),"brachiopod"]
d <- list("prePTME" = prePTME, "postPTME" = postPTME)
boxplot(d)

#### Testing best model to deal with overdispersion ####
## Read in functions for testing assumptions
source("functions/test.model.assumptions.R")
source("functions/test.spatial.autocorrelation.R")

## Define model - starting with poisson
model1 <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + n + (bivalve|stage), data = raw_data, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))
test.model.assumptions(model1)
# Dispersion: ratio 1.4067, p < 2.2e-16, significant.
# ZI: ratio 1.5607, p < 2.2e-16, significant.
# Outliers: expected 0.0059, observed 0.016, p < 2.2e-16, significant.
# Normality: p = 0.00028, significant.
# Collinearity: all VIFs below 2.5.

## Let's try negative binomial with classic paramterisation
model3 <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + n + (bivalve|stage), data = raw_data, family = nbinom2(link = "log"))
test.model.assumptions(model3)
# Dispersion: ratio 0.30109 p = 0.224, ininsignificant.
# ZI: ratio 1.7717, p < 2.2e-16, significant.
# Outliers: expected 0.0076, observed 0.0007, p = 0.08, insignificant.
# Normality: p = 0.0, significant.
# Collinearity: all VIFs below 2.5.

## Let's try negative binomial with quasi-Poisson parameterisation
model4 <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + n + (bivalve|stage), data = raw_data, family = nbinom1(link = "log"))
test.model.assumptions(model4)
# Dispersion: ratio 1.4468, p < 2.2e-16, significant.
# ZI: ratio 1.323, p < 2.2e-16, insignificant.
# Outliers: expected 0.0071, observed 0.0021, p = 0.28, insignificant.
# Normality: p = 0.00026, significant.
# Collinearity: all VIFs below 2.5.

## Any better with the third binomial model?
modelMixNB <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + n + (bivalve|stage), data = raw_data, family = nbinom12(link = "log"))
test.model.assumptions(modelMixNB)
# Dispersion: ratio 0.058928, p = 0.704, insignificant.
# ZI: ratio 1.1407, p =0.128, insignificant.
# Outliers: expected 0.0072, observed 0.0014, p = 0.06, insignificant.
# Normality: p = 0.0, significant.
# Collinearity: all VIFs below 2.5.

## Trying other models. Truncated negative binomial models are incompatible. Try Conway-Maxwell Poisson Distribution. Slow!
modelcompois <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + n + (bivalve|stage), data = raw_data, family = compois(link = "log"))
test.model.assumptions(modelcompois)
# Dispersion: ratio 0.064059, p < 2.2e-16, significant.
# ZI: ratio 1.5744, p < 2.2e-16, significant.
# Outliers: expected 0.0069, observed 0.0021, p = 0.4, insignificant.
# Normality: p = 0, significant.
# Collinearity: all VIFs below 2.5.

## Genpois
modelgenpois <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + n + (bivalve|stage), data = raw_data, family = genpois(link = "log"))
test.model.assumptions(modelgenpois)
# Dispersion: ratio 0.11663, p < 2.2e-16, significant
# ZI: ratio 1.3529, p < 2.2e-16, significant.
# Outliers: expected 0.0068, observed 0.0007, p = 0.04, significant.
# Normality: p = 0.0, significant.
# Collinearity: all VIFs below 2.5.

## This is my best model thus far. Let's try other link functions to see if there is any improvement. Let's check if different link functions will help.
bestModel <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + n + (bivalve|stage), data = raw_data, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))

## Test all model assumptions of bestModel so far
test.model.assumptions(bestModel)
# Dispersion: ratio 1.4067, p < 2.2e-16, significant.
# ZI: ratio 1.5607, p < 2.2e-16, significant.
# Outliers: expected 0.0059, observed 0.016, p < 2.2e-16, significant.
# Normality: p = 0.00028, significant.
# Collinearity: all VIFs below 2.5.

## Try sqrt link function
bestModel_sqrt <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + n + (bivalve|stage), data = raw_data, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))
test.model.assumptions(bestModel_sqrt)
# Dispersion: ratio 1.4075, p < 2.2e-16, significant. Worse.
# ZI: ratio 1.2359,  p < 2.2e-16, significant. Better.
# Outliers: expected 0.0057, observed 0.01, p < 2.2e-16, significant. Same.
# Normality: p = 0.01313, significant. A bit better.
# Collinearity: All VIFs below 2.5 except for bivalve:PTME (4.57) and bivalve (5.55). Worse.

## Best model fails all tests but plots look okay superficially (residual vs predictions plot has a patch). Will move forward to assess post-dredge.

#### Testing for spatial autocorrelation using DHARMa ####
test.spatial.autocorrelation(bestModel, raw_data)
## Expected -0.0007, observed = 0.0049, p = 0.001466, significant. However, no visible correlation from plot.

#### Dredge to see if we retain everything ####
## First, re-define model so as to specify na.activity. We include interaction between bivalves and PTME to see if relationship was reset.
model5 <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + n + (bivalve|stage), data = raw_data, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"), na.action = "na.fail")

## Get summary
tab_model(model5)

## Trying with two-way interactions first, will try to add. Sticking with AICc due to relatively small sample sizes in some stages.
combos <- dredge(model5, rank = "AICc")
## Export
saveRDS(combos, file = "data/sensitivity_testing/genera_CO_CRV_dredge_results.Rds")

## Get models with delta AIC of 2 or less
bestModel <- get.models(combos, delta<2)

## Three models with delta AIC less than 2
## Explore best models. Test for convergence and assumptions.
summary(bestModel[[1]])
bestModel_1 <- glmer(brachiopod ~ bivalve + PTME + bath + reef + lith + AbsLat + n + (bivalve|stage), data = raw_data, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))
test.model.assumptions(bestModel_1)
# Dispersion: ratio 1.4602, p < 2.2e-16, significant.
# ZI: ratio 1.5643, p < 2.2e-16, significant.
# Outliers: expected 0.0059, observed 0.015, p < 2.2e-16, significant.
# Normality: p = 0.0, significant.
# Collinearity: all VIFs below 2.5.
test.spatial.autocorrelation(bestModel_1, raw_data)
## Expected -0.00078, observed = 0.00754, p = 0.000328, significant. However, no visible correlation from plot.

summary(bestModel[[2]])
bestModel_2 <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + n + (bivalve|stage), data = raw_data, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))
test.model.assumptions(bestModel_2)
# Dispersion: ratio 1.4067, p < 2.2e-16, significant.
# ZI: ratio 1.5607, p < 2.2e-16, significant.
# Outliers: expected 0.0059, observed 0.016, p < 2.2e-16, significant.
# Normality: p = 0.00028, significant.
# Collinearity: all VIFs below 2.5.
test.spatial.autocorrelation(bestModel_2, raw_data)
## Expected -0.0007, observed = 0.0049, p = 0.001466, significant. However, no visible correlation from plot.

summary(bestModel[[3]])
bestModel_3 <- glmer(brachiopod ~ bivalve + PTME + bath + lith + AbsLat + n + (bivalve|stage), data = raw_data, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))
test.model.assumptions(bestModel_3)
# Dispersion: ratio 1.4657, p < 2.2e-16, significant.
# ZI: ratio 1.567, p < 2.2e-16, significant.
# Outliers: expected 0.0059, observed 0.021, p < 2.2e-16, significant.
# Normality: p = 1e-05, significant.
# Collinearity: all VIFs below 2.5.
test.spatial.autocorrelation(bestModel_3, raw_data)
## Expected -0.00078, observed = 0.005225, p = 0.0095, significant. However, no visible correlation from plot.

#### Plotting spatially standardised richness ####
## Read in clean data
richPlotData <- read.csv("data/analysis_data/genera_CO_CRV.csv", row.names = 1)

## Read in stages
stages <- read.csv("data/metadata/binning_timescale.csv")
stages$number <- seq(1,92,1)

## Add midpoints to data
richPlotData$midpoint <- stages[match(richPlotData$stage,stages$number),"midpoint"]

## Isolate stages and midpoints
richness <- richPlotData[,c(2,14)]
richness <- richness[!duplicated(richness),]

## Get median, 5th quantile, and 95th quantile
richness$bivalve_med <- NA
richness$bivalve_5th <- NA
richness$bivalve_95th <- NA
richness$brachiopod_med <- NA
richness$brachiopod_5th <- NA
richness$brachiopod_95th <- NA
richness$bivalve_mean <- NA
richness$bivalve_plus1sd <- NA
richness$bivalve_minus1sd <- NA
richness$brachiopod_mean <- NA
richness$brachiopod_plus1sd <- NA
richness$brachiopod_minus1sd <- NA

## Get stages in file
stages <- richness$stage

for(b in stages){
  ## Isolate relevant occurrences
  bin <- richPlotData[which(richPlotData$stage==b),]
  ## bivalve information
  richness[which(richness$stage == b),"bivalve_med"] <- median(bin[,"bivalve"])
  richness[which(richness$stage == b),"bivalve_5th"] <- quantile(bin[,"bivalve"], probs = 0.05)
  richness[which(richness$stage == b),"bivalve_95th"] <- quantile(bin[,"bivalve"], probs = 0.95)
  richness[which(richness$stage == b),"bivalve_mean"] <- mean(bin[,"bivalve"])
  richness[which(richness$stage == b),"bivalve_plus1sd"] <- mean(bin[,"bivalve"])+sd(bin[,"bivalve"])
  richness[which(richness$stage == b),"bivalve_minus1sd"] <- mean(bin[,"bivalve"])-sd(bin[,"bivalve"])
  ## brachiopod information
  richness[which(richness$stage == b),"brachiopod_med"] <- median(bin[,"brachiopod"])
  richness[which(richness$stage == b),"brachiopod_5th"] <- quantile(bin[,"brachiopod"], probs = 0.05)
  richness[which(richness$stage == b),"brachiopod_95th"] <- quantile(bin[,"brachiopod"], probs = 0.95)
  richness[which(richness$stage == b),"brachiopod_mean"] <- mean(bin[,"brachiopod"])
  richness[which(richness$stage == b),"brachiopod_plus1sd"] <- mean(bin[,"brachiopod"])+sd(bin[,"brachiopod"])
  richness[which(richness$stage == b),"brachiopod_minus1sd"] <- mean(bin[,"brachiopod"])-sd(bin[,"brachiopod"])
}

## Any NAs
richness[which(is.na(richness$bivalve_plus1sd)),"bivalve_plus1sd"] <- richness[which(is.na(richness$bivalve_plus1sd)),"bivalve_mean"]
richness[which(is.na(richness$bivalve_minus1sd)),"bivalve_minus1sd"] <- richness[which(is.na(richness$bivalve_minus1sd)),"bivalve_mean"]
richness[which(is.na(richness$brachiopod_plus1sd)),"brachiopod_plus1sd"] <- richness[which(is.na(richness$brachiopod_plus1sd)),"brachiopod_mean"]
richness[which(is.na(richness$brachiopod_minus1sd)),"brachiopod_minus1sd"] <- richness[which(is.na(richness$brachiopod_minus1sd)),"brachiopod_mean"]

## Any negatives, convert to 0
richness[which(richness$bivalve_minus1sd<0),"bivalve_minus1sd"] <- 0
richness[which(richness$brachiopod_minus1sd<0),"brachiopod_minus1sd"] <- 0

## Order by stage
richness <- richness[order(richness$stage),]

## Plot
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_CO_CRV_spatially_standardised_richness.pdf")
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = richness$midpoint, y = richness$bivalve_mean, main = "Spatially standardised generic richness (mean +/- 1 SD)\nClassical rarefaction (variable sample size)\n 100km grid cells, original covariate data only", axes = FALSE, xlim = c(520, 0), ylim = c(0,75), yaxs="i", xaxs="i",
     xlab = "Time (mya)", ylab = "Generic richness", type = "l", col = rgb(red = 1, green = 0, blue = 0, alpha = 0.75))
polygon(c(rev(richness$midpoint), richness$midpoint), c(rev(richness$bivalve_plus1sd), richness$bivalve_minus1sd), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.25), border = NA)
lines(x = richness$midpoint, y = richness$brachiopod_mean, xlim = c(520, 0), ylim = c(0,75),
      xlab = NA, ylab = "", type = "l", col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75))
polygon(c(rev(richness$midpoint), richness$midpoint), c(rev(richness$brachiopod_plus1sd), richness$brachiopod_minus1sd), col = rgb(red = 0, green = 0, blue = 1, alpha = 0.25), border = NA)
box()
axis(2)
legend("topleft",legend = c("Bivalves", "Brachiopods"),bg = "white", col = c(rgb(red = 1, green = 0, blue = 0, alpha = 0.75),col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75)),lty = 1)
axis_geo(side = 1, intervals = "international periods")
dev.off()

#### Plotting richness by group ####
## Get clean data for plotting
plotting.data <- read.csv("data/analysis_data/genera_CO_CRV.csv", row.names = 1)

## Round data
plotting.data$bivalve <- round(plotting.data$bivalve, digits = 0)
plotting.data$brachiopod <- round(plotting.data$brachiopod, digits = 0)

## Re-do model
plotModel <- glmmTMB(brachiopod ~ bivalve + (bivalve|stage), data = plotting.data, family = nbinom1(link = "log"))

## Get model data
line.df <- get_model_data(plotModel, type = "pred", terms = "bivalve")

## Assign color and shapes to data
prePTMErows <- which(plotting.data$PTME == "PrePTME")
postPTMErows <- which(plotting.data$PTME == "PostPTME")
plotting.data$PTME_color <- NA
plotting.data$PTME_shape <- NA
plotting.data[prePTMErows, "PTME_color"] <- "darkblue"
plotting.data[prePTMErows, "PTME_shape"] <- 4
plotting.data[postPTMErows, "PTME_color"] <- "darkorange"
plotting.data[postPTMErows, "PTME_shape"] <- 19

## Get unique point and color vectors
point.col <- unique(plotting.data$PTME_color)
point.shape <- unique(plotting.data$PTME_shape)
legend.shape <- point.shape[c(2,1)]

## Define plot title
plot.title <- "Generic richness from 100km grid cells\nClassical rarefaction (variable sample size), only original covariate data"

## Plot model bit by bit
## basic scatter plot
scatter <- ggplot() +
  xlab("Bivalve generic richness") +
  ylab("Brachiopod generic richness") +
  ggtitle(plot.title) +
  geom_point(data = plotting.data, aes(x = bivalve, y = brachiopod, colour = PTME, shape = PTME), ) +
  scale_color_manual(breaks = unique(plotting.data[,"PTME"]), values = point.col) +
  scale_shape_manual(values = point.shape) +
  geom_line(data = line.df, aes(x = x, y = predicted), linetype = "dashed") +
  geom_ribbon(data = line.df, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3)+
  scale_x_continuous(expand = c(0,0), limits = c(0,120)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,120)) +
  guides(shape = "none",
         color = guide_legend(override.aes = list(shape = legend.shape))) +
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
        legend.key.size = unit(10,"point"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))
scatter

## Export
pdf(file = paste0("figures/final/main/genera_CO_CRV_richness.pdf"))
print(scatter)
dev.off()

#### Plotting coefficients for top models ####
## Define new title
plot.title2 <- "Model coefficients from 100km grid cells\nClassical rarefaction (variable sample size), original covariate data only"

## Define axis labels and colours
axis.labels <- c("Bathymetry", "Generic\nbivalve\nrichness", "Lithology", "PTME", "Reefs", "Generic\nbivalve\nrichness\n+ PTME", "Absolute\nlatitude", "Sample\nsize")
colours <- c("lightblue", "darkgreen", "purple", "darkgrey", "pink", "orange", "cyan", "navy")
term <- c("bath", "bivalve", "lith", "PTMEPostPTME", "reef", "bivalve:PTMEPostPTME", "AbsLat", "n")
visualsRef <- data.frame(cbind("term" = term, "labels" = axis.labels, "colour" = colours))

## Get model data for each of the best models
coeffs <- lapply(1:length(bestModel), function(x){
  out <- get_model_data(bestModel[[x]], type = "est", transform = NULL)
})

## Function to reorgaise visualsRef into right order
reorganise_visualsRef <- function(visualsRef, coeffData){
  out <- visualsRef[rev(match(coeffData[,"term"], visualsRef[,"term"])),]
}

## Now run function
visuals <- lapply(1:length(bestModel), function(x){
  out <- reorganise_visualsRef(visualsRef, coeffs[[x]])
})

## Generate plots
plots <- lapply(1:length(bestModel), function(x){
  p <- ggplot() +
    scale_fill_manual(values = visuals[[x]][,"colour"]) +
    scale_color_manual(values = visuals[[x]][,"colour"]) +
    scale_x_discrete(labels = visuals[[x]][,"labels"]) +
    scale_y_continuous(expand = c(0,0), limits = c(-2.5,1)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
    geom_point(data = coeffs[[x]], (aes(x = term, y = estimate, fill = term, colour = term))) +
    geom_errorbar(data = coeffs[[x]], aes(x = term, ymin = conf.low, ymax = conf.high, colour = term), width=0.01) +
    ylab("Log-Odds") +
    xlab("") +
    theme(text = element_text(family = "Helvetica"),
          title = element_text(size = 12),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "none")
})

## Plot as grid
grid <- plot_grid(plotlist = plots, labels = LETTERS[1:length(bestModel)])
title <- ggdraw() + draw_label(plot.title2, fontface='bold')
output <- plot_grid(title, grid, ncol=1, rel_heights=c(0.075, 1)) # rel_heights values control title margins
## Check
output

## Export
pdf("figures/final/main/genera_CO_CRV_coefficients.pdf", width = 13)
print(output)
dev.off()

