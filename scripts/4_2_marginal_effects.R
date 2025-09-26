## 4.1.4 GLMM - genera, original covariates, CR20
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

## Standardise predictors
raw_data <- std(raw_data, raw_data[,c(2, 6, 7, 8, 9)])

## Drop non-standardized predictors
raw_data <- raw_data[,c(-2, -6, -7, -8, -9)]

## Re-do names
colnames(raw_data) <- c("stage", "brachiopod", "long", "lat", "PTME", "bivalve", "lith", "bath", "reef", "AbsLat")

## Clean up model data
modelData <- raw_data[,c(1,2,5,6,7,8,9,10)]

#### Define models ####
## Re-do best model just to make sure it has no convergence issues. Drops absolute latitude to help with convergence
bestModel_CO_CR20 <- glmer(brachiopod ~ bivalve * PTME + bath + reef + lith + (bivalve|stage), data = modelData, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))

#### CO_CR20 marginal effects ####
## PTME
## Predict response.
pr <- predict_response(bestModel_CO_CR20, "PTME", type = "fixed", ci_level = 0.95, bias_correction = T)

## Standardise predicted brachiopod values
pr$stdBrach <- std(pr$predicted)

## Correct standard error for new standard deviation
pr$stdStd.error <- (pr$std.error/sd(pr$predicted))*sd(pr$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
df <- insight::get_df(bestModel_CO_CR20, model.type = "wald", verbose = F)

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
postPTME_data <- modelData[which(modelData$PTME=="PostPTME"),]
postPTME_data$PTME <- NULL

prePTME_data <- modelData[which(modelData$PTME=="PrePTME"),]
prePTME_data$PTME <- NULL

## Re-do model - dropping to random intercept so both models will converge. Shouldn't be an issue now PTME term has been removed.
postModel <- glmer(brachiopod ~ bivalve + bath + reef + lith + (1|stage), data = postPTME_data, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))
preModel <- glmer(brachiopod ~ bivalve + bath + reef + lith + (1|stage), data = prePTME_data, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))

## Predict response.
pre <- predict_response(preModel, c("bivalve"), type = "fixed", ci_level = 0.95, bias_correction = T)

## Standardise predicted brachiopod values
pre$stdBrach <- std(pre$predicted)

## Correct standard error for new standard deviation
pre$stdStd.error <- (pre$std.error/sd(pre$predicted))*sd(pre$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
df <- insight::get_df(bestModel_CO_CR20, model.type = "wald", verbose = F)

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
  xlab("Standardised bivalve generic richness") +
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
pre <- predict_response(postModel, c("bivalve"), type = "fixed", ci_level = 0.95, bias_correction = T)

## Standardise predicted brachiopod values
pre$stdBrach <- std(pre$predicted)

## Correct standard error for new standard deviation
pre$stdStd.error <- (pre$std.error/sd(pre$predicted))*sd(pre$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
df <- insight::get_df(bestModel_CO_CR20, model.type = "wald", verbose = F)

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
  xlab("Standardised bivalve generic richness") +
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
prBiv <- predict_response(bestModel_CO_CR20, "bivalve", type = "fixed", ci_level = 0.95, bias_correction = T)

## Standardise predicted brachiopod values
prBiv$stdBrach <- std(prBiv$predicted)

## Correct standard error for new standard deviation
prBiv$stdStd.error <- (prBiv$std.error/sd(prBiv$predicted))*sd(prBiv$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
df <- insight::get_df(bestModel_CO_CR20, model.type = "wald", verbose = F)

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
  xlab("Standardised generic bivalve richness") +
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
prBath <- predict_response(bestModel_CO_CR20, "bath", type = "fixed", ci_level = 0.95, bias_correction = T)

## Standardise predicted brachiopod values
prBath$stdBrach <- std(prBath$predicted)

## Correct standard error for new standard deviation
prBath$stdStd.error <- (prBath$std.error/sd(prBath$predicted))*sd(prBath$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
df <- insight::get_df(bestModel_CO_CR20, model.type = "wald", verbose = F)

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
  xlab("Standardised proportion of samples from deep water environments") +
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
prLith <- predict_response(bestModel_CO_CR20, "lith", type = "fixed", ci_level = 0.95, bias_correction = T)

## Standardise predicted brachiopod values
prLith$stdBrach <- std(prLith$predicted)

## Correct standard error for new standard deviation
prLith$stdStd.error <- (prLith$std.error/sd(prLith$predicted))*sd(prLith$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
df <- insight::get_df(bestModel_CO_CR20, model.type = "wald", verbose = F)

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
  geom_ribbon(data=prLith, aes(ymin=std_lower_ci, ymax=std_upper_ci), alpha=0.8, fill="purple") +
  scale_x_continuous(expand = c(0,0), limits = c(min(prLith$x),max(prLith$x))) +
  scale_y_continuous(expand = c(0,0), limits = c(-3,3)) +
  geom_line(alpha=1, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  xlab("Standardised proportion of samples from carbonate sediments") +
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
prReef <- predict_response(bestModel_CO_CR20, "reef", type = "fixed", ci_level = 0.95, bias_correction = T)

## Standardise predicted brachiopod values
prReef$stdBrach <- std(prReef$predicted)

## Correct standard error for new standard deviation
prReef$stdStd.error <- (prReef$std.error/sd(prReef$predicted))*sd(prReef$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
df <- insight::get_df(bestModel_CO_CR20, model.type = "wald", verbose = F)

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
  xlab("Standardised proportion of samples from reef environments") +
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

#### Absolute latitude
## Predict response.
#prAbsLat <- predict_response(bestModel_CO_CR20, "AbsLat", type = "fixed", ci_level = 0.95, bias_correction = T)

## Standardise predicted brachiopod values
#prAbsLat$stdBrach <- std(prAbsLat$predicted)

## Correct standard error for new standard deviation
#prAbsLat$stdStd.error <- (prAbsLat$std.error/sd(prAbsLat$predicted))*sd(prAbsLat$stdBrach)

## Now to re-calculate confidence intervals to match
## Get degrees of freedom the same way as the function does
#df <- insight::get_df(bestModel_CO_CR20, model.type = "wald", verbose = F)

## Get z.score the same way. Use 95% confidence interval
#z_score <- qt((1-((1-0.95)/2)), df)

## Now need to re-calulate confidence intervals, as current values are weird.
#prAbsLat$conf.low <- prAbsLat$predicted - z_score * prAbsLat$std.error
#prAbsLat$conf.high <- prAbsLat$predicted + z_score * prAbsLat$std.error

## Get confidence intervals for regular data (check its working)
#prAbsLat$std_lower_ci <- prAbsLat$stdBrach - z_score * prAbsLat$stdStd.error
#prAbsLat$std_upper_ci <- prAbsLat$stdBrach + z_score * prAbsLat$stdStd.error

## And plot
#p5 <- ggplot(prAbsLat, aes(x, y = stdBrach)) +
#  geom_ribbon(data=prAbsLat, aes(ymin=std_lower_ci, ymax=std_upper_ci), alpha=0.8, fill="lightgreen") +
#  scale_x_continuous(expand = c(0,0), limits = c(min(prAbsLat$x),max(prAbsLat$x))) +
#  scale_y_continuous(expand = c(0,0), limits = c(-3,3)) +
#  geom_line(alpha=1, colour = "white") +
#  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
#  xlab("Standardised absolute latitude") +
#  ylab("Standardised brachiopod generic richness") +
#  ggtitle("Absolute Latitude") +
#  theme(text = element_text(family = "Helvetica"),
#        title = element_text(size = 12),
#        axis.text = element_text(size = 11),
#        axis.title = element_text(size = 12),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        panel.background = element_blank(),
#        axis.line = element_line(colour = "black"),
#        legend.position = "none")
#p5
#pdf("figures/final/main/genera_CO_CR20_marginal_AbsLat.pdf")
#print(p5)
#dev.off()
