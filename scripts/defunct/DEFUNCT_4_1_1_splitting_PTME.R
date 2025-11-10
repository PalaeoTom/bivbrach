#### Splitting up pre- and post-PTME ####
## Read in functions for testing assumptions
source("functions/test.model.assumptions.R")
source("functions/test.spatial.autocorrelation.R")

## Split up datasets
prePTME <- raw_data[which(raw_data[,"PTME"] == "PrePTME"),]
postPTME <- raw_data[which(raw_data[,"PTME"] == "PostPTME"),]

#### Pre-PTME ####
## Model fitting - try poisson distributed just in case it works. Square root link function worse.
poisson_prePTME <- glmer(brachiopod ~ bivalve + AbsLat + bivalve:AbsLat + (bivalve|stage), data = prePTME, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))
test.model.assumptions(poisson_prePTME)

## Try Nbinoms
NB2_prePTME <- glmmTMB(brachiopod ~ bivalve + AbsLat + bivalve:AbsLat + (bivalve|stage), data = prePTME, family = nbinom2(link = "log"))
test.model.assumptions(NB2_prePTME)

NB1_prePTME <- glmmTMB(brachiopod ~ bivalve + AbsLat + bivalve:AbsLat + (bivalve|stage), data = prePTME, family = nbinom1(link = "log"))
test.model.assumptions(NB1_prePTME)

NB12_prePTME <- glmmTMB(brachiopod ~ bivalve + AbsLat + bivalve:AbsLat + (bivalve|stage), data = prePTME, family = nbinom12(link = "log"))
test.model.assumptions(NB12_prePTME)

## PostPTME
poisson_postPTME <- glmer(brachiopod ~ bivalve + AbsLat + bivalve:AbsLat + (bivalve|stage), data = postPTME, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))
test.model.assumptions(poisson_postPTME)

## Try Nbinoms - convergence issue with NB2
NB2_postPTME <- glmmTMB(brachiopod ~ bivalve + AbsLat + bivalve:AbsLat + (bivalve|stage), data = postPTME, family = nbinom2(link = "log"))
test.model.assumptions(NB2_postPTME)

## Residuals bad
NB1_postPTME <- glmmTMB(brachiopod ~ bivalve + AbsLat + bivalve:AbsLat + (bivalve|stage), data = postPTME, family = nbinom1(link = "log"))
test.model.assumptions(NB1_postPTME)

NB12_postPTME <- glmmTMB(brachiopod ~ bivalve + AbsLat + bivalve:AbsLat + (bivalve|stage), data = postPTME, family = nbinom12(link = "log"))
test.model.assumptions(NB12_postPTME)

## None of these models are particularly good. Poisson is best. sqrt link function makes no difference.
bestModel_prePTME <- glmer(brachiopod ~ bivalve + AbsLat + (bivalve|stage), data = prePTME, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))
test.model.assumptions(bestModel_prePTME)
# Dispersion: ratio 1.0621, p = 2.2E-16, significant.
# ZI: ratio 8.9928, p < 2.2e-16, significant.
# Outliers: expected 0.007208, observed 0.01422, p = 0.02, significant.
# Normality: p = 5e-05, significant.
# Collinearity: All VIFs below 2.5.
test.spatial.autocorrelation(bestModel_prePTME, prePTME)
## Expected -0.000959, observed = 0.00314, p = 0.1112, insignificant.

## Create new plotting data
plotting.data <- read.csv("data/analysis_data/genera_NC_CR20.csv", header = T, row.names = 1)
plotting.data.prePTME <- plotting.data[which(plotting.data$PTME == "PrePTME"),]

## Round data
plotting.data.prePTME$bivalve <- round(plotting.data.prePTME$bivalve, digits = 0)
plotting.data.prePTME$brachiopod <- round(plotting.data.prePTME$brachiopod, digits = 0)

## Re-do model
plotModel <- glmer(brachiopod ~ bivalve + (bivalve|stage), data = plotting.data.prePTME, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))

## Get model data
line.df <- get_model_data(plotModel, type = "pred", terms = "bivalve")

## Define plot title
plot.title <- "Pre-PTME Generic richness from 100km grid cells\nClassical rarefaction (sample size = 20), no covariate data required"

## Plot model bit by bit
## basic scatter plot
scatter <- ggplot() +
  xlab("Bivalve generic richness") +
  ylab("Brachiopod generic richness") +
  ggtitle(plot.title) +
  geom_point(data = plotting.data.prePTME, aes(x = bivalve, y = brachiopod), shape = 19, colour = "darkblue") +
  geom_line(data = line.df, aes(x = x, y = predicted), linetype = "dashed") +
  geom_ribbon(data = line.df, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3)+
  scale_x_continuous(expand = c(0,0), limits = c(0,15)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,18)) +
  theme(text = element_text(family = "Helvetica"),
        title = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))
scatter

## Export
pdf(file = paste0("figures/final/main/genera_NC_CR20_prePTME_richness.pdf"))
print(scatter)
dev.off()

## Plotting coefficients
## Define new title
plot.title2 <- "Model coefficients from pre-PTME 100km grid cells\nClassical rarefaction (sample size = 20), no covariate data required"

## Define axis labels and colours
axis.labels <- c("Bathymetry", "Generic\nbivalve\nrichness", "Lithology", "PTME", "Reefs", "Generic\nbivalve\nrichness\n+ PTME", "Absolute\nlatitude")
colours <- c("lightblue", "darkgreen", "purple", "darkgrey", "pink", "orange", "cyan")
term <- c("bath", "bivalve", "lith", "PTMEPostPTME", "reef", "bivalve:PTMEPostPTME", "AbsLat")
visualsRef <- data.frame(cbind("term" = term, "labels" = axis.labels, "colour" = colours))

## Get model data for each of the best models
coeffs <- list(get_model_data(bestModel_prePTME, type = "est", transform = NULL))

## Function to reorgaise visualsRef into right order
reorganise_visualsRef <- function(visualsRef, coeffData){
  out <- visualsRef[rev(match(coeffData[,"term"], visualsRef[,"term"])),]
}
visuals <- lapply(1:length(coeffs), function(x){
  out <- reorganise_visualsRef(visualsRef, coeffs[[x]])
})

## Generate plots
plots <- lapply(1:length(coeffs), function(x){
  p <- ggplot() +
    scale_fill_manual(values = visuals[[x]][,"colour"]) +
    scale_color_manual(values = visuals[[x]][,"colour"]) +
    scale_x_discrete(labels = visuals[[x]][,"labels"]) +
    scale_y_continuous(expand = c(0,0), limits = c(-1.5,1)) +
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
grid <- plot_grid(plotlist = plots, labels = LETTERS[1:length(bestModel_prePTME)])
title <- ggdraw() + draw_label(plot.title2, fontface='bold')
output <- plot_grid(title, grid, ncol=1, rel_heights=c(0.075, 1)) # rel_heights values control title margins
## Check
output

## Export
pdf("figures/final/main/genera_NC_CR20_prePTME_coefficients.pdf", width = 13)
print(output)
dev.off()

#### Post-PTME ####
poisson_postPTME <- glmer(brachiopod ~ bivalve + AbsLat + (bivalve|stage), data = postPTME, family = poisson(link = "log"), control = glmerControl(optimizer="bobyqa"))
test.model.assumptions(poisson_postPTME)
## Sqrt link function worse.

## Try best negative binomial
NB2_postPTME <- glmmTMB(brachiopod ~ bivalve + AbsLat + (bivalve|stage), data = postPTME, family = nbinom2(link = "log"))
## Convergence issues

## Try other negative binomials
NB1_postPTME <- glmmTMB(brachiopod ~ bivalve + AbsLat + (bivalve|stage), data = postPTME, family = nbinom1(link = "log"))
test.model.assumptions(NB1_postPTME)
## sqrt link function worse

NB12_postPTME <- glmmTMB(brachiopod ~ bivalve + AbsLat + (bivalve|stage), data = postPTME, family = nbinom12(link = "log"))
test.model.assumptions(NB12_postPTME)
## Sqrt worse

## define best model
bestModel_postPTME <- glmmTMB(brachiopod ~ bivalve + AbsLat + (bivalve|stage), data = postPTME, family = nbinom12(link = "log"))
test.model.assumptions(bestModel_postPTME)
# Dispersion: ratio 0.55225, p = 0.024, significant but better ratio.
# ZI: ratio 1.1154, p = 0.048, significant.
# Outliers: expected 0.00656, observed 0, p = 0.08, insignificant.
# Normality: p = 0.429, insignificant.
# Collinearity: All VIFs below 2.5
test.spatial.autocorrelation(bestModel_postPTME, postPTME)
## Expected -0.000851, observed = 0.00260, p = 0.1445, insignificant.

## Create new plotting data
plotting.data <- read.csv("data/analysis_data/genera_NC_CR20.csv", header = T, row.names = 1)
plotting.data.postPTME <- plotting.data[which(plotting.data$PTME == "PostPTME"),]

## Round data
plotting.data.postPTME$bivalve <- round(plotting.data.postPTME$bivalve, digits = 0)
plotting.data.postPTME$brachiopod <- round(plotting.data.postPTME$brachiopod, digits = 0)

## Re-do model
plotModel <- glmmTMB(brachiopod ~ bivalve + (bivalve|stage), data = plotting.data.postPTME, family = nbinom12(link = "log"))

## Get model data
line.df <- get_model_data(plotModel, type = "pred", terms = "bivalve")

## Define plot title
plot.title <- "Post-PTME Generic richness from 100km grid cells\nClassical rarefaction (sample size = 20), no covariate data required"

## Plot model bit by bit
## basic scatter plot
scatter <- ggplot() +
  xlab("Bivalve generic richness") +
  ylab("Brachiopod generic richness") +
  ggtitle(plot.title) +
  geom_point(data = plotting.data.postPTME, aes(x = bivalve, y = brachiopod), shape = 4, colour = "darkorange") +
  geom_line(data = line.df, aes(x = x, y = predicted), linetype = "dashed") +
  geom_ribbon(data = line.df, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3)+
  scale_x_continuous(expand = c(0,0), limits = c(0,18)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,18)) +
  theme(text = element_text(family = "Helvetica"),
        title = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))
scatter

## Export
pdf(file = paste0("figures/final/main/genera_NC_CR20_postPTME_richness.pdf"))
print(scatter)
dev.off()

## Plotting coefficients
## Define new title
plot.title2 <- "Model coefficients from post-PTME 100km grid cells\nClassical rarefaction (sample size = 20), no covariate data required"

## Define axis labels and colours
axis.labels <- c("Bathymetry", "Generic\nbivalve\nrichness", "Lithology", "PTME", "Reefs", "Generic\nbivalve\nrichness\n+ PTME", "Absolute\nlatitude")
colours <- c("lightblue", "darkgreen", "purple", "darkgrey", "pink", "orange", "cyan")
term <- c("bath", "bivalve", "lith", "PTMEPostPTME", "reef", "bivalve:PTMEPostPTME", "AbsLat")
visualsRef <- data.frame(cbind("term" = term, "labels" = axis.labels, "colour" = colours))

## Get model data for each of the best models
coeffs <- list(get_model_data(bestModel_postPTME, type = "est", transform = NULL))

## Function to reorgaise visualsRef into right order
reorganise_visualsRef <- function(visualsRef, coeffData){
  out <- visualsRef[rev(match(coeffData[,"term"], visualsRef[,"term"])),]
}
visuals <- lapply(1:length(coeffs), function(x){
  out <- reorganise_visualsRef(visualsRef, coeffs[[x]])
})

## Generate plots
plots <- lapply(1:length(coeffs), function(x){
  p <- ggplot() +
    scale_fill_manual(values = visuals[[x]][,"colour"]) +
    scale_color_manual(values = visuals[[x]][,"colour"]) +
    scale_x_discrete(labels = visuals[[x]][,"labels"]) +
    scale_y_continuous(expand = c(0,0), limits = c(-1.5,1)) +
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
grid <- plot_grid(plotlist = plots, labels = LETTERS[1:length(bestModel_postPTME)])
title <- ggdraw() + draw_label(plot.title2, fontface='bold')
output <- plot_grid(title, grid, ncol=1, rel_heights=c(0.075, 1)) # rel_heights values control title margins
## Check
output

## Export
pdf("figures/final/main/genera_NC_CR20_postPTME_coefficients.pdf", width = 13)
print(output)
dev.off()
