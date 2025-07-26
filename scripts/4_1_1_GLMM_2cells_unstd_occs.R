## 4.1.1 GLMM (main analyses - 50km grid cells, 100km radius, 2 cells per sample, unstandardised occurrences)
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
raw_data <- read.csv("data/analysis_data/genera_2cell_raw.csv", header = T, row.names = 1)

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
colnames(raw_data) <- c("stage", "bivalve", "brachiopod", "long", "lat", "lith", "bath", "reef", "AbsLat", "PTME")

## Some counts are decimal. Round these to discrete values
raw_data$bivalve <- round(raw_data$bivalve, digits = 0)
raw_data$brachiopod <- round(raw_data$brachiopod, digits = 0)

## Retain a copy of data that is not standardised
raw_data_unstd <- raw_data

## Rearrange to match new order
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
## Define model
model1 <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = poisson(link = "log"))
model2 <- update(model1, control = glmerControl(optimizer="bobyqa"))

## Testing for overdispersion using DHARMa
## Simulate conditional on fitted random effects (more sensitive for mixed models)
simulationOutput2 <- simulateResiduals(model2, refit = F, re.form = NULL)
## Check with DHARMa simulation approach
testDispersion(simulationOutput2, type = "DHARMa")
## Very overdispersed - significant ratio of 3.29 well over 1.

## Let's try negative binomial with classic paramterisation
model3 <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom2(link = "log"))
## Testing for overdispersion using DHARMa once again
simulationOutput3 <- simulateResiduals(model3, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutput3, type = "DHARMa")
## Now significantly underdispersed! Ratio of 0.0004

## Let's try negative binomial with quasi-Poisson parameterisation
model4 <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom1(link = "log"))
## Testing for overdispersion using DHARMa once again
simulationOutput4 <- simulateResiduals(model4, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutput4, type = "DHARMa")
## Best by far. Dispersion is 0.79851.

## Any better with the third binomial model?
modelMixNB <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom12(link = "log"))
## Testing for overdispersion using DHARMa once again
simulationOutputMixNB <- simulateResiduals(modelMixNB, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutputMixNB, type = "DHARMa")
## nbinom1 seems the best (new ratio is 0.74). Dispersion of 0.79851 is the one to beat!

## Trying other models. Truncated negative binomial models are incompatible. Try Conway-Maxwell Poisson Distribution. Slow!
modelcompois <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = compois(link = "log"))
## Testing for overdispersion using DHARMa once again
simulationOutputcompois <- simulateResiduals(modelcompois, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutputcompois, type = "DHARMa")
## No better. Dispersion ratio is 0.16987

## Genpois
modelgenpois <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = genpois(link = "log"))
## Testing for overdispersion using DHARMa once again
simulationOutputgenpois <- simulateResiduals(modelgenpois, re.form = NULL)
## Test with DHARMa approach again
testDispersion(simulationOutputgenpois, type = "DHARMa")
## No better. Dispersion ratio is 0.4732.

## This is my best model. Let's try other link functions to see if there is any improvement. Let's check if different link functions will help.
bestModel <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom1(link = "log"))

## Logit, probit, cloglog failed to converge. Inverse and identity fail. Only two that converge without issue: log and sqrt
bestModel_log <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom1(link = "log"))
bestModel_sqrt <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom1(link = "sqrt"))

## Simulate residuals
simulated_log <- simulateResiduals(bestModel_log, re.form = NULL)
simulated_sqrt <- simulateResiduals(bestModel_sqrt, re.form = NULL)

## Check dispersion
testDispersion(simulated_log, type = "DHARMa")
testDispersion(simulated_sqrt, type = "DHARMa")
## Sqrt significantly underdispersed (ratio = 0.024809). Sticking with link = "log" as link function.

## Check all assumptions at this point
resBestModel <- simulateResiduals(bestModel, re.form = NULL)
testDispersion(resBestModel, type = "DHARMa") ## We know this already. 0.79851
testZeroInflation(resBestModel) ## Insignificant, but maybe a little zero inflation. Ratio is 1.1285
testOutliers(resBestModel, type = "bootstrap") ## Insignificant (p = 0.34). Outliers = 0.0048 observed versus 0.0071 expected
plot(resBestModel) ## Significant deviation from normality. Quantiles generally find but outliers.
check_collinearity(bestModel) ## Greatest VIF is bivalve at 2.52. All low.

## Can we make this better? What about accounting for decreasing variability with time on account of lower mean? What about adding a flat probability of producing a structural zero because of wider ecological range of bivalves?
bestModel_disp <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), dispformula = ~stage, data = raw_data, family = nbinom1(link = "log"))
bestModel_zero <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), ziformula = ~1, data = raw_data, family = nbinom1(link = "log"))
bestModel_zeroDisp <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), ziformula = ~1, dispformula = ~stage, data = raw_data, family = nbinom1(link = "log"))

## Simulate residuals for both and then test
resbestModel_disp <- simulateResiduals(bestModel_disp, re.form = NULL)
testDispersion(resbestModel_disp, type = "DHARMa") ## Highly insignificant but dispersion parameter lower at 0.39682!
testZeroInflation(resbestModel_disp) ## Ratio is 1.112. A tiny bit better.
testOutliers(resbestModel_disp, type = "bootstrap") ## Insignificant (p = 0.18). Outliers 0.0041 observed versus 0.00766 expected.
plot(resbestModel_disp) ## Normality failure. However, plots look overall pretty good. Probably just a couple of outliers.
check_collinearity(bestModel_disp) ## VIFs better (bivalve = 2.46).

resbestModel_zero <- simulateResiduals(bestModel_zero, re.form = NULL)
testDispersion(resbestModel_zero, type = "DHARMa") ## Ratio is 0.74428. Marginally worse!
testZeroInflation(resbestModel_zero) ## Ratio is 1.1235. Again, marginally worse.
testOutliers(resbestModel_zero, type = "bootstrap") ## Marginally worse. Expected (0.0067) versus observed (0.0034).
plot(resbestModel_zero) ## Still fine. KS-test failed but not really an issue.
check_collinearity(bestModel_zero) ## A little worse (bivalve VIF is 2.56) but still fine.

resbestModel_zeroDisp <- simulateResiduals(bestModel_zeroDisp, re.form = NULL)
testDispersion(resbestModel_zeroDisp, type = "DHARMa") ## Dispersion ratio is 0.94632. P-value = 0.736. Much better.
testZeroInflation(resbestModel_zeroDisp) ## Ratio is 1.0952. P-value = 0.152. Better.
testOutliers(resbestModel_zeroDisp, type = "bootstrap") ## A bit worse. Observed 0.0021 versus expected 0.0073. Significant (p = 0.02)
plot(resbestModel_zeroDisp) ## However, this outlier test is fine. KS-test fails but due to sampling size. Quantiles look better but some outliers.
check_collinearity(bestModel_zeroDisp) ## VIF marginally better (bivalves = 2.50).

## Best model has default dispersion and zero-inflation models.
bestModel <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), ziformula = ~1, dispformula = ~stage, data = raw_data, family = nbinom1(link = "log"))
resBestModel <- simulateResiduals(bestModel, re.form = NULL)

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

## Recalculate residuals for each grid cell
resBestModel2 <- recalculateResiduals(resBestModel, raw_data$cell, mean)

## Now test for spatialautocorrelation
testSpatialAutocorrelation(resBestModel2, x = groupLocations$cellLong, y = groupLocations$cellLat)

## Insignificant t p-value. Actual difference between expected (-0.0007) and observed (0.0023) is tiny. Plot looks fine.

#### Dredge to see if we retain everything ####
## Clean up model data
modelData <- raw_data[,c(1,2,5,6,7,8,9,10)]

## First, re-define model so as to specify na.activity. We include interaction between bivalves and PTME to see if relationship was reset.
model5 <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = modelData, ziformula = ~1, dispformula = ~stage, family = nbinom1(link = "log"), na.action = "na.fail")

## Get summary
tab_model(model5)

## Trying with two-way interactions first, will try to add. Sticking with AICc due to relatively small sample sizes in some stages.
combos <- dredge(model5, rank = "AICc")
## Don't worry about convergence warnings,

## Get best model
bestModel <- get.models(combos, delta==0)[[1]]
summary(bestModel)

## Re-do best model just to make sure it has no convergence issues
bestModel <- glmmTMB(brachiopod ~ bivalve * PTME + bath + lith + (bivalve|stage), data = modelData, ziformula = ~1, dispformula = ~stage, family = nbinom1(link = "log"))

## Test assumptions once again to be sure!
resBestModel <- simulateResiduals(bestModel, re.form = NULL)
testDispersion(resBestModel, type = "DHARMa") ## Insignificant. Ratio is 0.90684.
testZeroInflation(resBestModel) ## Insignificant, but a little better. Ratio is 1.0941
testOutliers(resBestModel, type = "bootstrap") ## Insignificant and about the same (p = 0.64). Outliers = 0.0055 observed versus 0.00729 expected
plot(resBestModel) ## "Significant" deviation from normality. Quantiles generally find but outliers.
check_collinearity(bestModel) ## Greatest VIF is bivalve at 2.46. Perfect!

## Spatial autocorrelation once again
## Recalculate residuals for each grid cell
resBestModel2 <- recalculateResiduals(resBestModel, raw_data$cell, mean)
## Now test for spatialautocorrelation
testSpatialAutocorrelation(resBestModel2, x = groupLocations$cellLong, y = groupLocations$cellLat)

## Insignificant but only just (p=0.0852). However, inspecting the plot shows no real correlation in residuals and difference between observed (0.0026) and expected (-0.0007) is negligible.

#### Calculate marginal effects ####
## Get model data before standardisation
modelData_unstd <- raw_data_unstd[,c(1,2,5,6,7,8,9,10)]

## Fit best model to unstandardised data
bestModel_raw <- glmmTMB(brachiopod ~ bivalve * PTME + bath + lith + (bivalve|stage), ziformula = ~1, dispformula = ~stage, data = modelData_unstd, family = nbinom1(link = "log"))

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
pdf("figures/final/main/genera_2cell_raw_marginal_PTME.pdf")
print(p1)
dev.off()

#### Bivalve
## Split by PTME only!
postPTME_data <- modelData_unstd[which(modelData_unstd$PTME=="PostPTME"),]
postPTME_data$PTME <- NULL

prePTME_data <- modelData_unstd[which(modelData_unstd$PTME=="PrePTME"),]
prePTME_data$PTME <- NULL

## Re-do model - dropping to random intercept so both models will converge. Shouldn't be an issue now PTME term has been removed.
postModel <- glmmTMB(brachiopod ~ bivalve + bath + lith + (1|stage), ziformula = ~1, dispformula = ~stage, data = postPTME_data, family = nbinom1(link = "log"))
preModel <- glmmTMB(brachiopod ~ bivalve + bath + lith + (1|stage), ziformula = ~1, dispformula = ~stage, data = prePTME_data, family = nbinom1(link = "log"))

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
pdf("figures/final/main/genera_2cell_raw_marginal_bivalve_prePTME.pdf")
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
  scale_y_continuous(expand = c(0,0), limits = c(-6,6)) +
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
pdf("figures/final/main/genera_2cell_raw_marginal_bivalve_postPTME.pdf")
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
pdf("figures/final/main/genera_2cell_raw_marginal_bathymetry.pdf")
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
pdf("figures/final/main/genera_2cell_raw_marginal_lithology.pdf")
print(p5)
dev.off()

#### Plotting coefficients and richness by group ####
## Get clean data for plotting
plotting.data <- read.csv("data/analysis_data/genera_2cell_raw.csv", row.names = 1)

## Round data
plotting.data$bivalve <- round(plotting.data$bivalve, digits = 0)
plotting.data$brachiopod <- round(plotting.data$brachiopod, digits = 0)

## Re-do model
plotModel <- glmmTMB(brachiopod ~ bivalve, data = plotting.data, ziformula = ~1, dispformula = ~stage, family = nbinom1(link = "log"), na.action = "na.fail")

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
plot.title <- "Generic richness from raw occurrences\nTwo 50km grid cells per sample"

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
  scale_x_continuous(expand = c(0,0), limits = c(0,200)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
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
pdf(file = paste0("figures/final/main/genera_2cell_raw_richness.pdf"))
print(scatter)
dev.off()

## Coefficient plot
## Define new title
plot.title2 <- "Model coefficients from raw occurrences\nTwo 50km grid cells per sample"
axis.labels <- c("Bathymetry", "Generic\nbivalve\nrichness", "Lithology", "PTME", "Generic\nbivalve\nrichness\n+ PTME")

## Colour scale
colours <- c("lightblue", "darkgreen", "brown", "purple", "darkblue")

## Get model data
coeff_data <- get_model_data(bestModel, type = "est")

## Drop dispersion parameter result.
coeff_data <- coeff_data[-nrow(coeff_data),]

## Re-order colour and label vectors
axis.labels <- axis.labels[c(5,3,1,4,2)]
colours <- colours[c(5,3,1,4,2)]

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
pdf("figures/final/main/genera_2cell_raw_coefficients.pdf")
print(p7)
dev.off()

#### Plotting richness ####
## Read in clean data
richPlotData <- read.csv("data/analysis_data/genera_2cell_raw.csv", row.names = 1)

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
pdf(file = "figures/final/supplemental/genera_2cell_spatially_standardised_richness_raw.pdf")
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(x = richness$stage_midpoint, y = richness$bivalve_mean, main = "Spatially standardised generic richness (mean +/- 1 SD)\nRaw occurrences\nTwo 50km grid cells per sample", axes = FALSE, xlim = c(520, 0), ylim = c(0,75), yaxs="i", xaxs="i",
     xlab = "Time (mya)", ylab = "Generic richness", type = "l", col = rgb(red = 1, green = 0, blue = 0, alpha = 0.75))
polygon(c(rev(richness$stage_midpoint), richness$stage_midpoint), c(rev(richness$bivalve_plus1sd), richness$bivalve_minus1sd), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.25), border = NA)
lines(x = richness$stage_midpoint, y = richness$brachiopod_mean, xlim = c(520, 0), ylim = c(0,75),
      xlab = NA, ylab = "", type = "l", col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75))
polygon(c(rev(richness$stage_midpoint), richness$stage_midpoint), c(rev(richness$brachiopod_plus1sd), richness$brachiopod_minus1sd), col = rgb(red = 0, green = 0, blue = 1, alpha = 0.25), border = NA)
box()
axis(2)
legend("topleft",legend = c("Bivalves", "Brachiopods"),bg = "white", col = c(rgb(red = 1, green = 0, blue = 0, alpha = 0.75),col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75)),lty = 1)
axis_geo(side = 1, intervals = "international periods")
dev.off()

