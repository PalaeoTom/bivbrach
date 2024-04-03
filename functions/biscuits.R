biscuits <- function(dat, xy, r, seeding = NULL, standardiseSiteN = T, rep = 100, nSite = 3,
                     threshold = 1, weight = FALSE,
                     returnSeeds = F, crs = "epsg:4326", output = "locs"){
  coords <- as.data.frame(divvy::uniqify(dat, xy))
  coords$id <- paste0("loc", 1:nrow(coords))
  if(is.null(seeding)){
    allPools <- findSeeds2(coords, "id", xy, r, nSite, crs, threshold)
  } else {
    allPools <- findSeeds2(coords, "id", xy, r, nSite, crs, threshold, seeding)
  }
  if (length(allPools) < 1) {
    stop("not enough close sites for any subsample or all cookies exceed overlap threshold. Please adjust nSite or thresh.")
  }
  seeds <- names(allPools)
  if(standardiseSiteN){
    subsamples <- replicate(rep, cookie(dat, seeds, xy, nSite, allPools, weight, coords, crs, output, standardiseSiteN, returnSeeds), simplify = FALSE)
    if(returnSeeds){
      usedSeeds <- sapply(1:length(subsamples), function(x) subsamples[[x]][[1]])
      seed_out <- coords[which(coords$id %in% usedSeeds),]
      subsamples_out <- lapply(1:length(subsamples), function(x) subsamples[[x]][[2]])
      names(subsamples_out) <- usedSeeds
      return(list("seeds" = seed_out, "subsamples" = subsamples_out))
    } else {
      return(subsamples)
    }
  } else {
    subsamples <- cookie(dat, seeds, xy, nSite, allPools, weight, coords, crs, output, standardiseSiteN)
    if(returnSeeds){
      names(subsamples) <- seeds
      seed_out <- coords[which(coords$id %in% seeds),]
      return(list("seeds" = seed_out, "subsamples" = subsamples))
    } else {
      return(subsamples)
    }
  }
}
