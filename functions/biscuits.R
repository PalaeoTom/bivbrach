biscuits <- function(dat, xy, r, seeding = NULL, iterate = T, rep = 100, nSite = 3, threshold = 1, weight = FALSE, returnSeeds = F, crs = "epsg:4326", output = "locs"){
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
  if(iterate){
    if(length(seeds) > 1) {
      seed <- sample(sample(seeds), 1)
    } else {
      seed <- seeds
    }
    subsamples <- replicate(rep, cookie(dat, seed, xy, allPools, weight, coords, crs, output), simplify = FALSE)
    if(returnSeeds){
      seed_out <- coords[which(coords$id %in% seeds),]
      return(list("seeds" = seed_out, "subsamples" = subsamples))
    } else {
      return(subsamples)
    }
  } else {
    subsamples <- lapply(1:length(allPools), function(x) coords[which(coords$id %in% allPools[[x]]),])
    names(subsamples) <- seeds
    if(returnSeeds){
      seed_out <- coords[which(coords$id %in% seeds),]
      return(list("seeds" = seed_out, "subsamples" = subsamples))
    } else {
      return(subsamples)
    }
  }
}
