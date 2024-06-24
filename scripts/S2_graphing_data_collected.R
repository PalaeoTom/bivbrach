## S2 Getting performance data for different settings in terms of the number of viable cookies produced.
## Started by TJS on 08/01/2024

### Only useful when more data integrated so individual time bins may be compared ####
#source("functions/countUsable.R")
#source("functions/wrap.countUsable.R")

## get grid of variable combination
#varGrid <- expand.grid(siteQuotas, radii/1000)
#threshold.VC <- 5

## count usable bins and format
#stages.g200.UTBs <- wrap.countUsable(VC = stages.g200.VCs, threshold.VC, varGrid, c("siteQuota", "radius"))
#stages.g100.UTBs <- wrap.countUsable(VC = stages.g100.VCs, threshold.VC, varGrid, c("siteQuota", "radius"))
#stages.s200.UTBs <- wrap.countUsable(VC = stages.s200.VCs, threshold.VC, varGrid, c("siteQuota", "radius"))
#stages.s100.UTBs <- wrap.countUsable(VC = stages.s100.VCs, threshold.VC, varGrid, c("siteQuota", "radius"))

## get combination of siteQuotas and radii
#split.vars <- expand.grid(radii/1000, siteQuotas)
#colnames(split.vars) <- c("radius", "siteQuota")

## get plotting data
#source("functions/get.VC.plot.data.R")
#stages.g200.barData <- get.VC.plot.data(UTB = stages.g200.UTBs, split.vars = split.vars)
#stages.g100.barData <- get.VC.plot.data(UTB = stages.g100.UTBs, split.vars = split.vars)
#stages.s200.barData <- get.VC.plot.data(UTB = stages.s200.UTBs, split.vars = split.vars)
#stages.s100.barData <- get.VC.plot.data(UTB = stages.s100.UTBs, split.vars = split.vars)

## set output directory and read in plotting function
#output.dir <- "/Users/tjs/R_packages/R_projects/bivbrach/figures"
#source("functions/plot.UTBs.R")

## Plot figures - add labels in post
## Set input strings and output names
#stages.output.name <- c("stages_g200", "stages_g100",
#                        "stages_s200", "stages_s100")
#stages.input.strings <- c("stages.g200.barData", "stages.g100.barData",
#                          "stages.s200.barData", "stages.s100.barData")

## Plot stages data
#for(i in 1:length(stages.input.strings)){
#  plot.UTBs(data = eval(parse(text=stages.input.strings[i])), output.dir = output.dir, output.name = stages.output.name[i])
#}
