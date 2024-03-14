cut.biscuits <- function(data, reps, siteQuota, r, biscuitThreshold = 1, b.crs, biscuitWeight = F, standardiseSiteNumber = T, b.xy = c("cellX", "cellY"), taxa = NULL, taxa.level = NULL){
  ## Derive a box of cookies
  box <- lapply(1:length(data), function(x){
    ## Return the error message
    attempt <- tryCatch(biscuits(dat = data[[x]],
                                xy = b.xy, seeding = NULL, standardiseSiteN = standardiseSiteNumber, rep = reps,
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
  return(box)
}
