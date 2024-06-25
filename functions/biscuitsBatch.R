biscuitsBatch <- function(dataList, siteQuota, r, b.crs, output.dir,
                         b.xy = c("cellX", "cellY"),
                         overlapThreshold = 0, overlapType = "area", overlapPruningMode = "maxOccs",
                         reps = 100, nOccs = 100,
                         rarefaction = "sitesThenOccs",
                         n.cores = 1, name.output = "new",
                         taxa = NULL, taxa.level = NULL){
  ## Set home directory
  home <- getwd()
  ## Get all possible combinations of settings (siteQuota, radius, overlapThreshold, overlapType, weightedStandardisation)
  settings <- expand.grid(siteQuota, r, overlapThreshold, overlapType)
  ## define identifiers for different settings and create list of settings for labelling
  params <- list(siteQuota, r, overlapThreshold, overlapType)
  labs <- list(paste0("sQ",seq(1,length(siteQuota),1)), paste0("r",seq(1,length(r),1)), paste0("oTh",seq(1,length(overlapThreshold),1)), paste0("oTy",seq(1,length(overlapType),1)))
  ## identify invariant elements to clean up labels
  vary <- apply(settings, 2, function(x) length(unique(x))) > 1
  ## label params with labs
  for (n in 1:length(params)) names(params[[n]]) <- labs[[n]]
  ## for each row in settings, run an analysis and export
  for(i in 1:nrow(settings)){
    ## Derive a box of cookies
    box <- mclapply(1:length(dataList), mc.cores = n.cores, function(x){
      ## Return the error message
      attempt <- tryCatch(biscuits(dataMat = dataList[[x]],
                                  xy = b.xy, seeding = NULL, rarefaction = rarefaction, reps = reps,
                                  nSites = settings[i,1], nOccs = nOccs, oThreshold = settings[i,3], oType = as.character(settings[i,4]), oPruningMode = overlapPruningMode,
                                  r = settings[i,2],
                                  crs = b.crs, returnSeeds = F, output = 'full'), error = function(e){})
      ## If it works, keep output, if not, return NA
      if(!is.null(attempt)){
        pack <- attempt
      } else {
        pack <- NA
      }
    })
    ## If taxa is not null, split output into specified taxa
    if(!is.null(taxa)){
      if(rarefaction == "divvySites" || rarefaction == "weightedDivvySites" || rarefaction == "none"){
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
      } else {
        if(rarefaction == "sites" || rarefaction == "occs" || rarefaction == "sitesThenOccs"){
          #### START HERE. The below needs to take into account extra level of structure in box object when using these three methods of rarefaction.
          box <- lapply(1:length(taxa), function(t){
            timeBin <- lapply(1:length(box), function(b){
              #b = 15
              if(!any(is.na(box[[b]]))){
                subsamples <- lapply(1:length(box[[b]]), function(s){
                #s = 1
                  pack <- lapply(1:length(box[[b]][[s]]), function(p){
                      cookie <- box[[b]][[s]][[p]][which(box[[b]][[s]][[p]][,taxa.level[t]] %in% taxa[t]),]
                  })
                })
              } else {
                pack <- NA
              }
            })
          })
          names(box) <- taxa
        } else {
          stop("check rarefaction argument is an accepted string")
        }
      }
    }
    ## Use sapply to pass over labs and params list to get output file names
    setwd(output.dir)
    saveRDS(box, file = paste0(name.output, "_", paste(sapply(1:ncol(settings[i,]), function(x) names(params[[x]][params[[x]] %in% settings[i,x]]))[vary], collapse = "_"), ".Rds"))
    setwd(home)
  }
}
