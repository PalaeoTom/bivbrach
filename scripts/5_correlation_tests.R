## 5. Correlation tests
## Started by TJS on 08/01/2024

## Clean directory
rm(list = ls())

## If packages aren't installed, install them, then load them
packages <- c("")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}

## Load variable vectors to read in files
radii <- as.integer(c(100000, 500000, 1000000, 2000000))
siteQuotas <- c(3, 6, 9, 12, 15)
overlapThresholds <- c(0, 0.25, 0.5, 0.75, 1)
overlapTypes <- c("area", "sites")
weightStandardisation_1 <- F
vars <- list(paste0("sQ",seq(1,length(siteQuotas),1)), paste0("r",seq(1,length(radii),1)), paste0("oTh",seq(1,length(overlapThresholds),1)), paste0("oTy",seq(1,length(overlapTypes),1)))

## Get midpoints from original data
setwd("~/R_packages/R_projects/bivbrach")
home <- getwd()
genera.10ma <- readRDS("data/BB_genera_10maBins.Rds")
genera.stages <- readRDS("data/BB_genera_stageBins.Rds")
times.10ma <- names(genera.10ma)
times.stag <- names(genera.stages)

## Set input and output directories
input.dir <- "~/OneDrive - Nexus365/Bivalve_brachiopod/data/raw_regRich"
output.dir <- "~/R_packages/R_projects/bivbrach/data"
input.pre <- "BB_gen_stag_raw_SQS"
output.pre <- "BB_gen_stag_raw_SQS_SR"
times = times.stag
threshold = 15

mass.correlation.test <- function(input.dir, input.pre, output.dir, output.pre, vars, times, n.cores = 1, threshold = 10, singleNAs = "correct", singleNAsValue = 0){
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".Rds")
  output.dirs <- paste0(output.dir, "/", output.pre, "_", varStrings, ".Rds")
  ## create output matrix
  output <- matrix(NA, ncol = length(varStrings), nrow = length(times))
  rownames(output) <- times
  colnames(output) <- varStrings
  ## populate output matrix
  for(i in 1:length(input.dirs)){
    data <- suppressWarnings(tryCatch(readRDS(input.dirs[i]), error = function(e){}))
    if(is.null(data)){
      next
    } else {
      ## get sampled time bins
      samp.times <- names(data)
      ## pass over list and prune out NAs
      for(x in 1:length(data)){
        data[[x]] <- data[[x]][!apply(data[[x]], 1, function(y) all(is.na(y))),]
      }
      ## deal with any remaining NAs
      if(any(sapply(1:length(data), function(z) any(is.na(data[[z]]))))){
        if(singleNAs == "correct"){
          for(n in 1:length(data)){
            data[[n]][which(is.na(data[[n]]))] <- singleNAsValue
          }
        } else {
          if(singleNAs = "drop"){
            for(n in 1:length(data)){
              is.na(data[[n]])
            }

          } else {
            stop("argument singleNAs needs to be 'correct' or 'drop'")
          }
        }
      }

    }
  }
}
