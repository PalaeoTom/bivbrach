## 4.1 GLMM (sensitivity test - 50km grid cells, 100km radius, 3 cells per sample, unstandardised occurrences)
## Started by TJS on 27/06/2025

#### Startup ####
## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("lme4", "MuMIn", "sjmisc", "glmmTMB", "DHARMa", "performance", "car", "divDyn", "gstat", "raster", "terra", "xtable", "sjPlot", "ggplot2", "palaeoverse", "ggeffects")
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

## Load data
raw_data <- read.csv("data/analysis_data/genera_3cell_raw.csv", header = T, row.names = 1)

#### Prepping data ####
## Drop columns not being used in model
raw_data <- raw_data[,c(-2, -11)]

## Check numeric entries are numeric
nums <- c(2:9)
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
colnames(raw_data) <- c("stage", "bivalve", "brachiopod", "lith", "bath", "reef", "AbsLat", "lat", "long", "PTME")

## Some counts are decimal. Round these to discrete values
raw_data$bivalve <- round(raw_data$bivalve, digits = 0)
raw_data$brachiopod <- round(raw_data$brachiopod, digits = 0)

## Retain a copy of data that is not standardised
raw_data_unstd <- raw_data

## Rearrange to match new order
raw_data_unstd <- raw_data_unstd[,c(1,3,8,9,10,2,4,5,6,7)]

## Standardise predictors
raw_data <- std(raw_data, raw_data[,c(2, 4, 5, 6, 7)])

## Drop non-standardized predictors
raw_data <- raw_data[,c(-2, -4, -5, -6, -7)]

## Re-do names
colnames(raw_data) <- c("stage", "brachiopod", "lat", "long", "PTME", "bivalve", "lith", "bath", "reef", "AbsLat")
colnames(raw_data_unstd) <- c("stage", "brachiopod", "lat", "long", "PTME", "bivalve", "lith", "bath", "reef", "AbsLat")

#### Explore relationship between variables and brachiopod richness ####
plot(x = raw_data$bivalve, y = raw_data$brachiopod)
plot(x = raw_data$lith, y = raw_data$brachiopod)
plot(x = raw_data$bath, y = raw_data$brachiopod)
plot(x = raw_data$reef, y = raw_data$brachiopod)
plot(x = raw_data$AbsLat, y = raw_data$brachiopod)
## Pre/Post-PTME
prePTME <- raw_data[which(raw_data$PTME == "PrePTME"),"brachiopod"]
postPTME <- raw_data[which(raw_data$PTME == "PostPTME"),"brachiopod"]
d <- list("prePTME" = prePTME, "postPTME" = postPTME)
boxplot(d)

#### Testing best model to deal with overdispersion ####
## Define model
model1 <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = poisson(link = "log"))
model2 <- update(model1, control = glmerControl(optimizer="bobyqa"))

## Testing for overdispersion using DHARMa
## Simulate conditional on fitted random effects (more sensitive for mixed models)
simulationOutput <- simulateResiduals(model2, refit = F, re.form = NULL)
## Check with DHARMa simulation approach
testDispersion(simulationOutput, type = "DHARMa")
## Very overdispersed - significant ratio of 4.6 well over 1.

## Let's try negative binomial with classic paramterisation
model3 <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom2(link = "log"))
## Testing for overdispersion using DHARMa once again
simulationOutput2 <- simulateResiduals(model3, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutput2, type = "DHARMa")
## Now significantly underdispersed!

## Let's try negative binomial with quasi-Poisson parameterisation
model4 <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom1(link = "log"))
## Testing for overdispersion using DHARMa once again
simulationOutput3 <- simulateResiduals(model4, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutput3, type = "DHARMa")
## Now insignificantly underdispersed!

## Any better with the third binomial model?
modelMixNB <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom12(link = "log"))
## Testing for overdispersion using DHARMa once again
simulationOutputMixNB <- simulateResiduals(modelMixNB, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutputMixNB, type = "DHARMa")
## Dispersion closer to 1, still insignificant. Rolling with nbinom12

#### Check for multicollinearity using performance package ####
## Define model without interaction term
modelMC <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom12(link = "log"))
check_collinearity(modelMC)

## VIF is very low. Bivalve is highest at 2.56, so just above 2.5 threshold. Keeping each.

#### Testing for spatial autocorrelation using DHARMa ####
## First we need to group by location - lets use 50km grid cells, as this is the grain of our analysis
rPrj <- terra::project(x = terra::rast(), y =  "EPSG:8857",
                       res = 50000)
terra::values(rPrj) <- 1:terra::ncell(rPrj)
llOccs <- terra::vect(raw_data, geom = c("long","lat"), crs = "EPSG:8857")
raw_data[, "cell"] <- terra::cells(rPrj, llOccs)[, "cell"]
raw_data[, c("cellLong", "cellLat")] <- terra::xyFromCell(rPrj, raw_data$cell)

## Extract and remove duplicates
groupLocations <- raw_data[,c(11:13)]
groupLocations <- groupLocations[!duplicated(groupLocations),]

## Then recalculate residuals for each grid cell
simulationOutput4 <- recalculateResiduals(simulationOutputMixNB, raw_data$cell, mean)

## Now test for spatialautocorrelation
testSpatialAutocorrelation(simulationOutput4, x = groupLocations$cellLong, y = groupLocations$cellLat)

## None! Could break this down by time period:
## See distribution amongst time bins
## Most common is 10-15 points in a time bin.
hist(table(raw_data$stage))

## Will need to use simple model.

## Get stages
stages <- unique(raw_data$stage)

## Initialize containers
statistics <- c()
pvalues <- c()

for(s in stages){
  ## extract stages
  dat <- raw_data[which(raw_data$stage == s),]
  ## if all bivalve values the same, return NA
  if(length(unique(dat$bivalve)) == 1){
    statistics <- c(statistics, NA)
    pvalues <- c(pvalues, NA)
  } else {
    ## fit model
    modelSA <- glmmTMB(brachiopod ~ bivalve, data = dat, family = nbinom12(link = "log"))
    ## simulate data
    simulationSA <- simulateResiduals(modelSA, re.form = NULL)
    ## do test
    test <- testSpatialAutocorrelation(simulationSA, x = dat$long, y = dat$lat)
    ## record statistics
    statistics <- c(statistics,test$statistic[[1]])
    pvalues <- c(pvalues,test$p.value)
  }
}

## Which bivalve richness are spatially autocorrelated
stages[which(pvalues < 0.05)]

## Some spatial autocorrelation within individual bins. However, not worth overparameterising the model for the sake of this, especially when sample size too small for convergence.

#### Dredge to see if we retain everything ####
## Clean up model data
modelData <- raw_data[,c(1,2,5,6,7,8,9,10)]

## First, re-define model so as to specify na.activity. We include interaction between bivalves and PTME to see if relationship was reset.
model5 <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = modelData, family = nbinom12(link = "log"), na.action = "na.fail")

## Get summary
tab_model(model5)

## Trying with two-way interactions first, will try to add. Sticking with AICc due to relatively small sample sizes in some stages.
combos <- dredge(model5, rank = "AICc")

## Get best model
bestModel <- get.models(combos, delta==0)[[1]]
summary(bestModel)

## Test if model assumptions are sound
simulationOutputBest <- simulateResiduals(bestModel, re.form = NULL)

## Further tests
plot(simulationOutputBest)

## KS test indicates significant deviation but I'm not really concerned about that. More of a sample size effect.
## Other test
# Zero inflation, insignificant
testZeroInflation(simulationOutputBest)

#### Link functions and model assumptions ####
## Trying different link functions with model4 (our best model so far)
bestModel_log <- glmmTMB(brachiopod ~ bivalve * PTME + bath + lith + (bivalve|stage), data = modelData, family = nbinom12(link = "log"), na.action = "na.fail")
bestModel_logit <- glmmTMB(brachiopod ~ bivalve * PTME + bath + lith + (bivalve|stage), data = modelData, family = nbinom12(link = "logit"), na.action = "na.fail")
bestModel_probit <- glmmTMB(brachiopod ~ bivalve * PTME + bath + lith + (bivalve|stage), data = modelData, family = nbinom12(link = "probit"), na.action = "na.fail")
bestModel_inverse <- glmmTMB(brachiopod ~ bivalve * PTME + bath + lith + (bivalve|stage), data = modelData, family = nbinom12(link = "inverse"), na.action = "na.fail")
bestModel_cloglog <- glmmTMB(brachiopod ~ bivalve * PTME + bath + lith + (bivalve|stage), data = modelData, family = nbinom12(link = "cloglog"), na.action = "na.fail")
bestModel_identity <- glmmTMB(brachiopod ~ bivalve * PTME + bath + lith + (bivalve|stage), data = modelData, family = nbinom12(link = "identity"), na.action = "na.fail")
bestModel_sqrt <- glmmTMB(brachiopod ~ bivalve * PTME + bath + lith + (bivalve|stage), data = modelData, family = nbinom12(link = "sqrt"), na.action = "na.fail")

## Logit, probit, cloglog failed to converge. Inverse and identity failed. Simulate with two four.
simulated_log <- simulateResiduals(bestModel_log, re.form = NULL)
simulated_sqrt <- simulateResiduals(bestModel_sqrt, re.form = NULL)

## Check dispersion
testDispersion(simulated_log, type = "DHARMa")
testDispersion(simulated_sqrt, type = "DHARMa")
## Sqrt significant underdispersed. Sticking with link = "log"

## Check assumptions again.
plot(simulated_log)

#### Calculate marginal effects ####
## Get model data before standardisation
modelData_unstd <- raw_data_unstd[,c(1,2,5,6,7,8,9,10)]

## Fit best model to unstandardised data
bestModel_raw <- glmmTMB(brachiopod ~ bivalve * PTME + bath + lith + (bivalve|stage), data = modelData_unstd, family = nbinom12(link = "log"), na.action = "na.fail")

#### PTME
## Predict response.
pr <- predict_response(bestModel_raw, "PTME", type = "fixed", ci_level = 0.95)

## Standardise predicted brachiopod values
pr$stdBrach <- std(pr$predicted)

## Correct standard error for new standard deviation
pr$stdStd.error <- (pr$std.error/sd(pr$predicted))*sd(pr$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
df <- insight::get_df(bestModel_raw, model.type = "wald", verbose = F)

## Get z.score the same way. Use 95% confidence interval
z_score <- qt((1-((1-0.95)/2)), df)

## Now need to re-calulate confidence intervals, as current values are weird.
pr$conf.low <- pr$predicted - z_score * pr$std.error
pr$conf.high <- pr$predicted + z_score * pr$std.error

## Get confidence intervals for regular data (check its working)
pr$std_lower_ci <- pr$stdBrach - z_score * pr$stdStd.error
pr$std_upper_ci <- pr$stdBrach + z_score * pr$stdStd.error

## And plot
p1 <- ggplot() +
  scale_fill_manual(values = c("darkblue", "darkorange")) +
  scale_color_manual(values = c("darkblue", "darkorange")) +
  scale_x_discrete(labels = c("Pre-PTME", "Post-PTME")) +
  scale_y_continuous(expand = c(0,0), limits = c(-2,2)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  geom_point(data = pr, (aes(x = x, y = stdBrach, fill = x, colour = x))) +
  geom_errorbar(data = pr, aes(x = x, ymin = std_lower_ci, ymax = std_upper_ci, colour = x), width=0.01) +
  ylab("Standardised brachiopod generic richness") +
  xlab("") +
  ggtitle("Permian-Triassic Mass Extinction (PTME) event") +
  theme(text = element_text(family = "Helvetica"),
        title = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
p1
pdf("figures/final/main/genera_3cell_raw_marginal_PTME.pdf")
print(p1)
dev.off()

#### Bivalve
## Split by PTME only!
postPTME_data <- modelData_unstd[which(modelData_unstd$PTME=="PostPTME"),]
postPTME_data$PTME <- NULL

prePTME_data <- modelData_unstd[which(modelData_unstd$PTME=="PrePTME"),]
prePTME_data$PTME <- NULL

## Re-do model - dropping to random intercept for pre model to get it to converge now PTME term has been removed
postModel <- glmmTMB(brachiopod ~ bivalve + bath + lith + (bivalve|stage), data = postPTME_data, family = nbinom12(link = "log"), na.action = "na.fail")
preModel <- glmmTMB(brachiopod ~ bivalve + bath + lith + (1|stage), data = prePTME_data, family = nbinom12(link = "log"), na.action = "na.fail")

## Predict response.
pre <- predict_response(preModel, c("bivalve"), type = "fixed", ci_level = 0.95)

## Standardise predicted brachiopod values
pre$stdBrach <- std(pre$predicted)

## Correct standard error for new standard deviation
pre$stdStd.error <- (pre$std.error/sd(pre$predicted))*sd(pre$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
df <- insight::get_df(bestModel_raw, model.type = "wald", verbose = F)

## Get z.score the same way. Use 95% confidence interval
z_score <- qt((1-((1-0.95)/2)), df)

## Now need to re-calulate confidence intervals, as current values are weird.
pre$conf.low <- pre$predicted - z_score * pre$std.error
pre$conf.high <- pre$predicted + z_score * pre$std.error

## Get confidence intervals for regular data (check its working)
pre$std_lower_ci <- pre$stdBrach - z_score * pre$stdStd.error
pre$std_upper_ci <- pre$stdBrach + z_score * pre$stdStd.error

## And plot
p2 <- ggplot(pre, aes(x, y = stdBrach)) +
  geom_ribbon(data=pre, aes(ymin=std_lower_ci, ymax=std_upper_ci), alpha=0.8, fill="darkorange") +
  scale_x_continuous(expand = c(0,0), limits = c(min(pre$x),max(pre$x))) +
  scale_y_continuous(expand = c(0,0), limits = c(-2,2)) +
  geom_line(alpha=1, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  xlab("Bivalve generic richness") +
  ylab("Standardised brachiopod generic richness") +
  ggtitle("Bivalve generic richness (before the PTME)") +
  theme(text = element_text(family = "Helvetica"),
        title = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
p2
pdf("figures/final/main/genera_3cell_raw_marginal_bivalve_prePTME.pdf")
print(p2)
dev.off()

#### Bivalves after PTME
## Predict response.
pre <- predict_response(postModel, c("bivalve"), type = "fixed", ci_level = 0.95)

## Standardise predicted brachiopod values
pre$stdBrach <- std(pre$predicted)

## Correct standard error for new standard deviation
pre$stdStd.error <- (pre$std.error/sd(pre$predicted))*sd(pre$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
df <- insight::get_df(bestModel_raw, model.type = "wald", verbose = F)

## Get z.score the same way. Use 95% confidence interval
z_score <- qt((1-((1-0.95)/2)), df)

## Now need to re-calulate confidence intervals, as current values are weird.
pre$conf.low <- pre$predicted - z_score * pre$std.error
pre$conf.high <- pre$predicted + z_score * pre$std.error

## Get confidence intervals for regular data (check its working)
pre$std_lower_ci <- pre$stdBrach - z_score * pre$stdStd.error
pre$std_upper_ci <- pre$stdBrach + z_score * pre$stdStd.error

## And plot
p3 <- ggplot(pre, aes(x, y = stdBrach)) +
  geom_ribbon(data=pre, aes(ymin=std_lower_ci, ymax=std_upper_ci), alpha=0.8, fill="darkblue") +
  scale_x_continuous(expand = c(0,0), limits = c(min(pre$x),max(pre$x))) +
  scale_y_continuous(expand = c(0,0), limits = c(-4,4)) +
  geom_line(alpha=1, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  xlab("Bivalve generic richness") +
  ylab("Standardised brachiopod generic richness") +
  ggtitle("Bivalve generic richness (after the PTME)") +
  theme(text = element_text(family = "Helvetica"),
        title = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
p3
pdf("figures/final/main/genera_3cell_raw_marginal_bivalve_postPTME.pdf")
print(p3)
dev.off()

#### Bathymetry
## Predict response.
prBath <- predict_response(bestModel_raw, "bath", type = "fixed", ci_level = 0.95)

## Standardise predicted brachiopod values
prBath$stdBrach <- std(prBath$predicted)

## Correct standard error for new standard deviation
prBath$stdStd.error <- (prBath$std.error/sd(prBath$predicted))*sd(prBath$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
df <- insight::get_df(bestModel_raw, model.type = "wald", verbose = F)

## Get z.score the same way. Use 95% confidence interval
z_score <- qt((1-((1-0.95)/2)), df)

## Now need to re-calulate confidence intervals, as current values are weird.
prBath$conf.low <- prBath$predicted - z_score * prBath$std.error
prBath$conf.high <- prBath$predicted + z_score * prBath$std.error

## Get confidence intervals for regular data (check its working)
prBath$std_lower_ci <- prBath$stdBrach - z_score * prBath$stdStd.error
prBath$std_upper_ci <- prBath$stdBrach + z_score * prBath$stdStd.error

## And plot
p4 <- ggplot(prBath, aes(x, y = stdBrach)) +
  geom_ribbon(data=prBath, aes(ymin=std_lower_ci, ymax=std_upper_ci), alpha=0.8, fill="lightblue") +
  scale_x_continuous(expand = c(0,0), limits = c(min(prBath$x),max(prBath$x))) +
  scale_y_continuous(expand = c(0,0), limits = c(-2,2)) +
  geom_line(alpha=1, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  xlab("Proportion of samples from deep water environments") +
  ylab("Standardised brachiopod generic richness") +
  ggtitle("Bathymetry") +
  theme(text = element_text(family = "Helvetica"),
        title = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
p4
pdf("figures/final/main/genera_3cell_raw_marginal_bathymetry.pdf")
print(p4)
dev.off()

#### Lithology
## Predict response.
prLith <- predict_response(bestModel_raw, "lith", type = "fixed", ci_level = 0.95)

## Standardise predicted brachiopod values
prLith$stdBrach <- std(prLith$predicted)

## Correct standard error for new standard deviation
prLith$stdStd.error <- (prLith$std.error/sd(prLith$predicted))*sd(prLith$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
df <- insight::get_df(bestModel_raw, model.type = "wald", verbose = F)

## Get z.score the same way. Use 95% confidence interval
z_score <- qt((1-((1-0.95)/2)), df)

## Now need to re-calulate confidence intervals, as current values are weird.
prLith$conf.low <- prLith$predicted - z_score * prLith$std.error
prLith$conf.high <- prLith$predicted + z_score * prLith$std.error

## Get confidence intervals for regular data (check its working)
prLith$std_lower_ci <- prLith$stdBrach - z_score * prLith$stdStd.error
prLith$std_upper_ci <- prLith$stdBrach + z_score * prLith$stdStd.error

## And plot
p5 <- ggplot(prLith, aes(x, y = stdBrach)) +
  geom_ribbon(data=prLith, aes(ymin=std_lower_ci, ymax=std_upper_ci), alpha=0.8, fill="brown") +
  scale_x_continuous(expand = c(0,0), limits = c(min(prLith$x),max(prLith$x))) +
  scale_y_continuous(expand = c(0,0), limits = c(-2,2)) +
  geom_line(alpha=1, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  xlab("Proportion of samples from carbonate sediments") +
  ylab("Standardised brachiopod generic richness") +
  ggtitle("Lithology") +
  theme(text = element_text(family = "Helvetica"),
        title = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
p5
pdf("figures/final/main/genera_3cell_raw_marginal_lithology.pdf")
print(p5)
dev.off()

#### Plotting coefficients and richness by group ####
## Get clean data for plotting
plotting.data <- read.csv("data/analysis_data/genera_3cell_raw.csv", row.names = 1)

## Round data
plotting.data$bivalve <- round(plotting.data$bivalve, digits = 0)
plotting.data$brachiopod <- round(plotting.data$brachiopod, digits = 0)

## Re-do model
plotModel <- glmmTMB(brachiopod ~ bivalve, data = plotting.data, family = nbinom12(link = "log"), na.action = "na.fail")

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
plot.title <- "Generic richness, 50km grid cells, 100km radius, 2 cells"

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
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
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
        legend.key.size = unit(10,"point"))
scatter

## Export
pdf(file = paste0("figures/final/main/genera_3cell_raw_richness.pdf"))
print(scatter)
dev.off()

## Coefficient plot
## Define new title
plot.title2 <- "Model coefficients"
axis.labels <- c("Bathymetry", "Generic\nbivalve\nrichness", "Lithology", "PTME", "Generic\nbivalve\nrichness\n+ PTME")
axis.labels <- axis.labels[c(5,4,3,2,1)]

## Colour scale
colours <- c("lightblue", "darkgreen", "brown", "purple", "darkblue")

## Get model data
coeff_data <- get_model_data(bestModel, type = "est")

## And plot
p7 <- ggplot() +
  scale_fill_manual(values = colours) +
  scale_color_manual(values = colours) +
  scale_x_discrete(labels = axis.labels) +
  scale_y_continuous(expand = c(0,0), limits = c(0,2)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey") +
  geom_point(data = coeff_data, (aes(x = term, y = estimate, fill = term, colour = term))) +
  geom_errorbar(data = coeff_data, aes(x = term, ymin = conf.low, ymax = conf.high, colour = term), width=0.01) +
  ylab("") +
  xlab("") +
  ggtitle("Model coefficients") +
  theme(text = element_text(family = "Helvetica"),
        title = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
p7
pdf("figures/final/main/genera_3cell_raw_coefficients.pdf")
print(p7)
dev.off()

#### Plotting richness ####
## Read in clean data
richPlotData <- read.csv("data/analysis_data/genera_3cell_raw.csv", row.names = 1)

## Isolate stages and midpoints
richness <- richPlotData[,c(1,11)]
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

## Get stages
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

## Plot
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/final/supplemental/genera_3cell_spatially_standardised_richness.pdf")
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = richness$stage_midpoint, y = richness$bivalve_mean, main = "Spatially standardised generic richness (mean +/- 1 SD)", axes = FALSE, xlim = c(520, 0), ylim = c(0,50), yaxs="i", xaxs="i",
     xlab = NA, ylab = "Generic richness", type = "l", col = rgb(red = 1, green = 0, blue = 0, alpha = 0.75))
polygon(c(rev(richness$stage_midpoint), richness$stage_midpoint), c(rev(richness$bivalve_plus1sd), richness$bivalve_minus1sd), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.25), border = NA)
lines(x = richness$stage_midpoint, y = richness$brachiopod_mean, xlim = c(520, 0), ylim = c(0,50),
      xlab = NA, ylab = "", type = "l", col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75))
polygon(c(rev(richness$stage_midpoint), richness$stage_midpoint), c(rev(richness$brachiopod_plus1sd), richness$brachiopod_minus1sd), col = rgb(red = 0, green = 0, blue = 1, alpha = 0.25), border = NA)
box()
axis(2)
legend("topleft",legend = c("Bivalves", "Brachiopods"),bg = "white", col = c(rgb(red = 1, green = 0, blue = 0, alpha = 0.75),col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75)),lty = 1)
axis_geo(side = 1, intervals = "international periods")
dev.off()

