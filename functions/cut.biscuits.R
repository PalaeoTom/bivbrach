cut.biscuits <- function(data, reps, siteQuota, r, biscuitThreshold = 1, b.crs, biscuitWeight = F, b.xy = c("cellX", "cellY"), taxa = NULL, taxa.level = NULL, min.row = 2, richness = T){
  ## Derive a box of cookies
  box <- lapply(1:length(data), function(x){
    ## Return the error message
    attempt <- tryCatch(biscuits(dat = data[[x]],
                                xy = b.xy, seeding = NULL, standardiseSiteN = T, rep = reps,
                                nSite = siteQuota, threshold = biscuitThreshold,
                                r = r, weight = biscuitWeight,
                                crs = b.crs, returnSeeds = F, output = 'full'), error = function(e){})
    ## If it works, keep output, if not, return NA
    if(!is.null(attempt)){
      pack <- attempt
    } else {
      pack <- NA
    }
  })
  ## Drop cookies with fewer than minimum number of rows
  for(b in 1:length(box)){
    if(!any(is.na(box[[b]]))){
      drop.vector <- c()
      for(p in 1:length(box[[b]])){
        if(nrow(box[[b]][[p]]) < min.row) {
          drop.vector <- c(drop.vector,FALSE)
        } else {
          drop.vector <- c(drop.vector,TRUE)
        }
      }
      ## Use drop vector to prune out deficient cookies
      box[[b]] <- box[[b]][drop.vector]
    }
  }
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
  ## if output = "richness", calculate species richness for each cookies - assumes uniqified data as just counts rows
  if(richness){
    final <- lapply(1:length(box), function(t){
      part <- lapply(1:length(box[[t]]), function(b){
        if(!any(is.na(box[[t]][[b]]))){
          pack <- sapply(1:length(box[[t]][[b]]), function(p) nrow(box[[t]][[b]][[p]]))
        } else {
          pack <- NA
        }
      })
    })
    names(final) <- taxa
  } else {
    final <- box
  }
  return(final)
}
