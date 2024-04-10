cut.biscuits <- function(data, siteQuota, r, b.crs, output.dir,
                         b.xy = c("cellX", "cellY"),
                         overlapThreshold = 1, overlapType = "area",
                         reps = 100,
                         standardiseSiteNumber = T, weightedStandardisation = F, taxa = NULL, taxa.level = NULL,
                         n.cores = 1, name.output = "new"){
  ## Set home directory
  home <- getwd()
  ## Get all possible combinations of settings (siteQuota, radius, overlapThreshold, overlapType, weightedStandardisation)
  settings <- expand.grid(siteQuota, r, overlapThreshold, overlapType, weightedStandardisation)
  ## define identifiers for different settings and create list of settings for labelling
  params <- list(siteQuota, r, overlapThreshold, overlapType, weightedStandardisation)
  labs <- list(paste0("sQ",seq(1,length(siteQuota),1)), paste0("r",seq(1,length(r),1)), paste0("oTh",seq(1,length(overlapThreshold),1)), paste0("oTy",seq(1,length(overlapType),1)), paste0("wS",seq(1,length(weightedStandardisation),1)))
  ## identify invariant elements to clean up labels
  vary <- apply(settings, 2, function(x) length(unique(x))) > 1
  ## label params with labs
  for (n in 1:length(params)) names(params[[n]]) <- labs[[n]]
  ## for each row in settings, run an analysis and export
  for(i in 1:nrow(settings)){
    ## Derive a box of cookies
    box <- mclapply(1:length(data), mc.cores = n.cores, function(x){
      ## Return the error message
      attempt <- tryCatch(biscuits(dat = data[[x]],
                                  xy = b.xy, seeding = NULL, standardiseSiteN = standardiseSiteNumber, rep = reps,
                                  nSite = settings[i,1], oThreshold = settings[i,3], oType = as.character(settings[i,4]),
                                  r = settings[i,2], weight = settings[i,5],
                                  crs = b.crs, returnSeeds = F, output = 'full'), error = function(e){})
      ## If it works, keep output, if not, return NA
      if(!is.null(attempt)){
        pack <- attempt
      } else {
        pack <- NA
      }
    })
    ## Partition each pack of cookies into different taxa
    if(!is.null(taxa)){
      box <- lapply(1:length(taxa), function(t){
        part <- lapply(1:length(box), function(b){
          if(!any(is.na(box[[b]]))){
            pack <- lapply(1:length(box[[b]]), function(p){
              cookie <- box[[b]][[p]][which(box[[b]][[p]][,taxa.level[t]] %in% taxa[t]),]
            })
          } else {
            pack <- NA
          }
        })
      })
      names(box) <- taxa
    }
    ## Use sapply to pass over labs and params list to get output file names
    setwd(output.dir)
    saveRDS(box, file = paste0(name.output, "_", paste(sapply(1:ncol(settings[i,]), function(x) names(params[[x]][params[[x]] %in% settings[i,x]]))[vary], collapse = "_"), ".Rds"))
    setwd(home)
  }
}
