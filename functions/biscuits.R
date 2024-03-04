biscuits <- function(dat, xy, r, seeding = NULL, iterate = T, rep = 100, nSite = 3, threshold = 1, weight = FALSE, crs = "epsg:4326", output = "locs"){
  ## uniqify the data
  coords <- as.data.frame(uniqify(dat, xy))
  ## assign location ID
  coords$id <- paste0("loc", 1:nrow(coords))
  ## generate seeds
  if(is.null(seeding)){
    allPools <- findSeeds2(coords, "id", xy, r, nSite, crs, threshold)
  } else {
    allPools <- findSeeds2(coords, "id", xy, r, nSite, crs, threshold, seeding)
  }
  ## if less than one cookie, stop
  if (length(allPools) < 1) {
    stop("not enough close sites for any subsample or all cookies exceed overlap threshold. Please adjust nSite or thresh.")
  }
  ## Might need to change this - how cookie is replicated
  seeds <- names(allPools)
  ## Random sampling with replacement
  if(iterate){
    if(length(seeds) > 1) {
      seed <- sample(sample(seeds), 1)
    } else {
      seed <- seeds
    }
    replicate(rep, cookie(seed), simplify = FALSE)
  } else {
    lapply(1:length(seeds), function(x) cookie(seeds[x]))
  }
}
