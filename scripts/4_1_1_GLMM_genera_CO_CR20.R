## 4.1.1 GLMM - genera, covPrun, CR20
## Started by TJS on 27/06/2025

#### Startup ####
## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("lme4", "MuMIn", "sjmisc", "glmmTMB", "DHARMa", "performance", "car", "divDyn", "gstat", "raster", "terra", "xtable", "sjPlot", "ggplot2", "palaeoverse", "ggeffects", "stringr")
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

## Load data
raw_data <- read.csv("data/analysis_data/genera_CO_CR20.csv", header = T, row.names = 1)

#### Prepping data ####
## Drop columns not being used in model
raw_data <- raw_data[,c(-1, -3)]

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
colnames(raw_data) <- c("stage", "bivalve", "brachiopod", "long", "lat", "lith", "bath", "reef", "AbsLat", "PTME")

## Some counts are decimal. Round these to discrete values to work with negative binomial
raw_data$bivalve <- round(raw_data$bivalve, digits = 0)
raw_data$brachiopod <- round(raw_data$brachiopod, digits = 0)

## Retain a copy of data that is not standardised
raw_data_unstd <- raw_data

## Rearrange to match new order of standardised
raw_data_unstd <- raw_data_unstd[,c(1,3,4,5,10,2,6,7,8,9)]

## Standardise predictors
raw_data <- std(raw_data, raw_data[,c(2, 6, 7, 8, 9)])

## Drop non-standardized predictors
raw_data <- raw_data[,c(-2, -6, -7, -8, -9)]

## Re-do names
colnames(raw_data) <- c("stage", "brachiopod", "long", "lat", "PTME", "bivalve", "lith", "bath", "reef", "AbsLat")
colnames(raw_data_unstd) <- c("stage", "brachiopod", "long", "lat", "PTME", "bivalve", "lith", "bath", "reef", "AbsLat")

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
## Define model - starting with poisson
model1 <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))
model2 <- update(model1, control = glmerControl(optimizer="bobyqa"))

## Testing for overdispersion using DHARMa
## Simulate conditional on fitted random effects (more sensitive for mixed models)
simulationOutput2 <- simulateResiduals(model2, refit = F, re.form = NULL)
## Check with DHARMa simulation approach
testDispersion(simulationOutput2, type = "DHARMa")
## Result: ratio is 0.92577, insignificant.

## Let's try negative binomial with classic paramterisation
model3 <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom2(link = "log"))
model3 <- update(model3, control = glmmTMBControl(optimizer = bobyqa))
## Testing for overdispersion using DHARMa
simulationOutput3 <- simulateResiduals(model3, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutput3, type = "DHARMa")
## Result: ratio of 0.66257, significant.

## Let's try negative binomial with quasi-Poisson parameterisation
model4 <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom1(link = "log"))
## Testing for overdispersion using DHARMa
simulationOutput4 <- simulateResiduals(model4, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutput4, type = "DHARMa")
## Result: ratio of 0.5409, significant.

## Any better with the third binomial model?
modelMixNB <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom12(link = "log"))
## Testing for overdispersion using DHARMa
simulationOutputMixNB <- simulateResiduals(modelMixNB, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutputMixNB, type = "DHARMa")
## Result: ratio of 0.5409, significant.

## Trying other models. Truncated negative binomial models are incompatible. Try Conway-Maxwell Poisson Distribution. Slow!
modelcompois <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = compois(link = "log"))
## Testing for overdispersion using DHARMa
simulationOutputcompois <- simulateResiduals(modelcompois, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutputcompois, type = "DHARMa")
## Result: ratio of 0.61837. Significant.

## Genpois
modelgenpois <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = genpois(link = "log"))
## Testing for overdispersion using DHARMa o
simulationOutputgenpois <- simulateResiduals(modelgenpois, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutputgenpois, type = "DHARMa")
## Result: ratio of 0.52847, significant

## This is my best model thus far. Let's try other link functions to see if there is any improvement. Let's check if different link functions will help.
bestModel <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))

## Check all assumptions at this point
resBestModel <- simulateResiduals(bestModel, re.form = NULL)
testDispersion(resBestModel, type = "DHARMa") ## Ratio of 0.92577, insignificant.
testZeroInflation(resBestModel) ## Ratio of 1.1817, significant.
testOutliers(resBestModel, type = "bootstrap") ## Expect freq. of 0.00527 versus observed 0.00855. Insignificant.
plot(resBestModel) ## All fine (more sensitive outlier test didn't yield issues.)
check_collinearity(bestModel) ## VIF of PTME and bivalve:PTME are moderately correlated.

## Try sqrt link function
bestModel_sqrt <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))

## Check assumptions
resbestModel_sqrt <- simulateResiduals(bestModel_sqrt, re.form = NULL)
testDispersion(resbestModel_sqrt, type = "DHARMa") ## Ratio of 0.93795, insignificant. A bit better!
testZeroInflation(resbestModel_sqrt) ## Ratio of 1.1939, significant. Marginally worse.
testOutliers(resbestModel_sqrt, type = "bootstrap") ## Expect freq. of 0.0052 versus observed 0.0057. Insignificant. Better!
plot(resbestModel_sqrt) ## Normality fine, just dispersion. Quantiles look a smidge structured.
check_collinearity(bestModel_sqrt) ## Low VIFs! Perfect!

## Reset best model
bestModel <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))
resbestModel <- simulateResiduals(bestModel, re.form = NULL)

#### Testing for spatial autocorrelation using DHARMa ####
## First we need to group by location - lets use 50km grid cells, as this is the grain of our analysis
rPrj <- terra::project(x = terra::rast(), y =  "EPSG:8857",
                       res = 100000)
terra::values(rPrj) <- 1:terra::ncell(rPrj)
llOccs <- terra::vect(raw_data, geom = c("long","lat"), crs = "EPSG:8857")
raw_data[, "cell"] <- terra::cells(rPrj, llOccs)[, "cell"]
raw_data[, c("cellLong", "cellLat")] <- terra::xyFromCell(rPrj, raw_data$cell)

## Extract and remove duplicates
groupLocations <- raw_data[,c(11:13)]
groupLocations <- groupLocations[!duplicated(groupLocations),]

## Recalculate residuals for each grid cell
resBestModel2 <- recalculateResiduals(resBestModel, raw_data$cell, mean)

## Now test for spatialautocorrelation
testSpatialAutocorrelation(resBestModel2, x = groupLocations$cellLong, y = groupLocations$cellLat)

## p = 0.02786. However, no visible correlation from plot.

#### Dredge to see if we retain everything ####
## Clean up model data
modelData <- raw_data[,c(1,2,5,6,7,8,9,10)]

## First, re-define model so as to specify na.activity. We include interaction between bivalves and PTME to see if relationship was reset.
model5 <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = modelData, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"), na.action = "na.fail")

## Get summary
tab_model(model5)

## Trying with two-way interactions first, will try to add. Sticking with AICc due to relatively small sample sizes in some stages.
combos <- dredge(model5, rank = "AICc")
## Don't worry about convergence warnings,

## Get best model
bestModel <- get.models(combos, delta==0)[[1]]
summary(bestModel)

## Re-do best model just to make sure it has no convergence issues
bestModel <- glmer(brachiopod ~ bivalve + PTME + bath + reef + lith + (bivalve|stage), data = modelData, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))

## Test assumptions once again to be sure!
resBestModel <- simulateResiduals(bestModel, re.form = NULL)
testDispersion(resBestModel, type = "DHARMa") ## Ratio is 0.93287, insignificant.
testZeroInflation(resBestModel) ## Ratio is 1.193, significant.
testOutliers(resBestModel, type = "bootstrap") ## Insignificant. Observed 0.0057 versus 0.0048 expected.
plot(resBestModel) ## Nothing to worry about - outlier test resust is due to type 1 error rate inflation
check_collinearity(bestModel) ## Wonderfully low!

## Spatial autocorrelation once again
## Recalculate residuals for each grid cell
resBestModel2 <- recalculateResiduals(resBestModel, raw_data$cell, mean)
## Now test for spatialautocorrelation
testSpatialAutocorrelation(resBestModel2, x = groupLocations$cellLong, y = groupLocations$cellLat)

## Inspection shows no obvious correlation but lowish p-value of 0.3558 shows some. Undoubtedly a problem due to replicates.

#### Calculate marginal effects ####
## Get model data before standardisation
modelData_unstd <- raw_data_unstd[,c(1,2,5,6,7,8,9,10)]

## Fit best model to unstandardised data
bestModel_raw <- glmer(brachiopod ~ bivalve + PTME + bath + reef + lith + (bivalve|stage), data = modelData_unstd, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))

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
pdf("figures/final/main/genera_CO_CR20_marginal_PTME.pdf")
print(p1)
dev.off()

#### Bivalve
## Split by PTME only!
postPTME_data <- modelData_unstd[which(modelData_unstd$PTME=="PostPTME"),]
postPTME_data$PTME <- NULL

prePTME_data <- modelData_unstd[which(modelData_unstd$PTME=="PrePTME"),]
prePTME_data$PTME <- NULL

## Re-do model - dropping to random intercept so both models will converge. Shouldn't be an issue now PTME term has been removed.
postModel <- glmer(brachiopod ~ bivalve + bath + reef + lith + (1|stage), data = postPTME_data, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))
preModel <- glmer(brachiopod ~ bivalve + bath + reef + lith + (1|stage), data = prePTME_data, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))

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
  scale_y_continuous(expand = c(0,0), limits = c(-3,3)) +
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
pdf("figures/final/main/genera_CO_CR20_marginal_bivalve_prePTME.pdf")
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
  scale_y_continuous(expand = c(0,0), limits = c(-3,3)) +
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
pdf("figures/final/main/genera_CO_CR20_marginal_bivalve_postPTME.pdf")
print(p3)
dev.off()

## All bivalves
## Predict response.
prBiv <- predict_response(bestModel_raw, "bivalve", type = "fixed", ci_level = 0.95)

## Standardise predicted brachiopod values
prBiv$stdBrach <- std(prBiv$predicted)

## Correct standard error for new standard deviation
prBiv$stdStd.error <- (prBiv$std.error/sd(prBiv$predicted))*sd(prBiv$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
df <- insight::get_df(bestModel_raw, model.type = "wald", verbose = F)

## Get z.score the same way. Use 95% confidence interval
z_score <- qt((1-((1-0.95)/2)), df)

## Now need to re-calulate confidence intervals, as current values are weird.
prBiv$conf.low <- prBiv$predicted - z_score * prBiv$std.error
prBiv$conf.high <- prBiv$predicted + z_score * prBiv$std.error

## Get confidence intervals for regular data (check its working)
prBiv$std_lower_ci <- prBiv$stdBrach - z_score * prBiv$stdStd.error
prBiv$std_upper_ci <- prBiv$stdBrach + z_score * prBiv$stdStd.error

## And plot
p4 <- ggplot(prBiv, aes(x, y = stdBrach)) +
  geom_ribbon(data=prBiv, aes(ymin=std_lower_ci, ymax=std_upper_ci), alpha=0.8, fill= "darkgreen") +
  scale_x_continuous(expand = c(0,0), limits = c(min(prBiv$x),max(prBiv$x))) +
  scale_y_continuous(expand = c(0,0), limits = c(-3,3)) +
  geom_line(alpha=1, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  xlab("Generic bivalve richness") +
  ylab("Standardised brachiopod generic richness") +
  ggtitle("Generic bivalve richness") +
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
pdf("figures/final/main/genera_CO_CR20_marginal_bivalve_richness.pdf")
print(p4)
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
  scale_y_continuous(expand = c(0,0), limits = c(-3,3)) +
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
pdf("figures/final/main/genera_CO_CR20_marginal_bathymetry.pdf")
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
  scale_y_continuous(expand = c(0,0), limits = c(-3,3)) +
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
pdf("figures/final/main/genera_CO_CR20_marginal_lithology.pdf")
print(p5)
dev.off()

#### Reef
## Predict response.
prReef <- predict_response(bestModel_raw, "reef", type = "fixed", ci_level = 0.95)

## Standardise predicted brachiopod values
prReef$stdBrach <- std(prReef$predicted)

## Correct standard error for new standard deviation
prReef$stdStd.error <- (prReef$std.error/sd(prReef$predicted))*sd(prReef$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
df <- insight::get_df(bestModel_raw, model.type = "wald", verbose = F)

## Get z.score the same way. Use 95% confidence interval
z_score <- qt((1-((1-0.95)/2)), df)

## Now need to re-calulate confidence intervals, as current values are weird.
prReef$conf.low <- prReef$predicted - z_score * prReef$std.error
prReef$conf.high <- prReef$predicted + z_score * prReef$std.error

## Get confidence intervals for regular data (check its working)
prReef$std_lower_ci <- prReef$stdBrach - z_score * prReef$stdStd.error
prReef$std_upper_ci <- prReef$stdBrach + z_score * prReef$stdStd.error

## And plot
p5 <- ggplot(prReef, aes(x, y = stdBrach)) +
  geom_ribbon(data=prReef, aes(ymin=std_lower_ci, ymax=std_upper_ci), alpha=0.8, fill="pink") +
  scale_x_continuous(expand = c(0,0), limits = c(min(prReef$x),max(prReef$x))) +
  scale_y_continuous(expand = c(0,0), limits = c(-3,3)) +
  geom_line(alpha=1, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  xlab("Proportion of samples from reef environments") +
  ylab("Standardised brachiopod generic richness") +
  ggtitle("Reefs") +
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
pdf("figures/final/main/genera_CO_CR20_marginal_reefs.pdf")
print(p5)
dev.off()

#### Plotting coefficients and richness by group ####
## Get clean data for plotting
plotting.data <- read.csv("data/analysis_data/genera_CO_CR20.csv", row.names = 1)

## Round data
plotting.data$bivalve <- round(plotting.data$bivalve, digits = 0)
plotting.data$brachiopod <- round(plotting.data$brachiopod, digits = 0)

## Re-do model
plotModel <- glmer(brachiopod ~ bivalve + (bivalve|stage), data = plotting.data, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))

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
plot.title <- "Generic richness from 100km grid cells\nClassical rarefaction (sample size = 20), only original covariate data"

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
  scale_x_continuous(expand = c(0,0), limits = c(0,22)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,22)) +
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
pdf(file = paste0("figures/final/main/genera_CO_CR20_richness.pdf"))
print(scatter)
dev.off()

## Coefficient plot
## Define new title
plot.title2 <- "Model coefficients from 100km grid cells\nClassical rarefaction (sample size = 20), only original covariate data"
axis.labels <- c("Bathymetry", "Generic\nbivalve\nrichness", "Lithology", "PTME", "Reefs")

## Colour scale
colours <- c("lightblue", "darkgreen", "brown", "purple", "pink")

## Get model data
coeff_data <- get_model_data(bestModel, type = "est")

## Re-order colour and label vectors
axis.labels <- axis.labels[c(3,5,1,4,2)]
colours <- colours[c(3,5,1,4,2)]

## And plot
p7 <- ggplot() +
  scale_fill_manual(values = colours) +
  scale_color_manual(values = colours) +
  scale_x_discrete(labels = axis.labels) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey") +
  geom_point(data = coeff_data, (aes(x = term, y = estimate, fill = term, colour = term))) +
  geom_errorbar(data = coeff_data, aes(x = term, ymin = conf.low, ymax = conf.high, colour = term), width=0.01) +
  ylab("") +
  xlab("") +
  ggtitle(plot.title2) +
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
pdf("figures/final/main/genera_CO_CR20_coefficients.pdf")
print(p7)
dev.off()

#### Plotting richness ####
## Read in clean data
richPlotData <- read.csv("data/analysis_data/genera_CO_CR20.csv", row.names = 1)

## Read in stages
stages <- read.csv("data/metadata/binning_timescale.csv")
stages$number <- seq(1,92,1)

## Add midpoints to data
richPlotData$midpoint <- stages[match(richPlotData$stage,stages$number),"midpoint"]

## Isolate stages and midpoints
richness <- richPlotData[,c(2,13)]
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
pdf(file = "figures/final/supplemental/genera_CO_CR20_spatially_standardised_richness.pdf")
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = richness$midpoint, y = richness$bivalve_mean, main = "Spatially standardised generic richness of 100km grid cells (mean +/- 1 SD)\nClassical rarefaction (sample size = 20), original covariate data only", axes = FALSE, xlim = c(520, 0), ylim = c(0,20), yaxs="i", xaxs="i",
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

#### Sensitivity test - shuflling predictor and response. Can we break negative relationship? ####
## Shuffle model data
shuffled_data <- lapply(1:1000, function(x){
  out <- modelData
  out$brachiopod <- out$brachiopod[sample(1:length(out$brachiopod),length(out$brachiopod),replace = F)]
  return(out)
})

## Rerun best model for all
shuffled_data_models <- lapply(1:length(shuffled_data), function(y){
  model <- glmer(brachiopod ~ bivalve + PTME + bath + reef + lith + (bivalve|stage), data = shuffled_data, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))
})

## Count number of significant terms for each predictor with jumbled responses - hopefully low!
## Do first model for reference
coeffs1 <- get_model_data(shuffled_data_models[[1]], type = "est")
reps = 1000
tracker_coeff <- data.frame(matrix(NA, ncol = 5, nrow = reps))
tracker_pstars <- data.frame(matrix(NA, ncol = 5, nrow = reps))
colnames(tracker_coeff) <- coeffs1[,"term"]
colnames(tracker_pstars) <- coeffs1[,"term"]

for(i in 1:reps){
  ## get model data
  coeffs <- get_model_data(shuffled_data_models[[i]], type = "est")
  ## add coefficients
  tracker_coeff[i,which(colnames(tracker_coeff) == coeffs[,"term"])] <- coeffs[,"estimate"]
  ## Log pstars
  tracker_pstars[i,which(colnames(tracker_pstars) == coeffs[,"term"])] <- coeffs[,"p.stars"]
}

## Summarise - find all significant terms
length(which(str_detect(tracker_pstars[,"bivalve"], fixed("*"))))
length(which(str_detect(tracker_pstars[,"PTMEPostPTME"], fixed("*"))))
length(which((str_detect(tracker_pstars[,"bath"], fixed("*")))))
length(which(str_detect(tracker_pstars[,"lith"], fixed("*"))))
length(which(str_detect(tracker_pstars[,"reef"], fixed("*"))))
