findSeeds2 <- function(dat, siteId, xy, r, nSite, crs = "epsg:4326", threshold = 1, seeding = NULL){
  ## get vector of site IDs
  if(is.null(seeding)){
    sites <- dat[, siteId]
    ## convert dat to simple factors object
    datSf <- sf::st_as_sf(dat, coords = xy, crs = crs)
    ## convert radius to units
    rad <- units::set_units(r, "km")
    ## get sites for potential each seed
    posPools <- lapply(sites, function(s) {
      ## get seed row
      seedRow <- which(dat[, siteId] == s)[1]
      ## get pool
      sPool <- findPool2(seedRow, datSf, sites, xy, rad, crs)
      ## get number of sites
      n <- length(sPool)
      ## if equal to or more than quota, retain
      if (n >= nSite)
        sPool
    })
  } else {
    sites <- dat[seeding, siteId]
    ## convert dat to simple factors object
    datSf <- sf::st_as_sf(dat, coords = xy, crs = crs)
    ## convert radius to units
    rad <- units::set_units(r, "km")
    ## get sites for potential each seed
    posPools <- lapply(sites, function(s) {
      ## get seed row
      seedRow <- which(dat[, siteId] == s)[1]
      ## get pool
      sPool <- findPool2(seedRow, datSf, dat[,siteId], xy, rad, crs)
      ## get number of sites
      n <- length(sPool)
      ## if equal to or more than quota, retain
      if (n >= nSite)
        sPool
    })
  }
  ## Name each element of list
  names(posPools) <- sites
  ## filter out those which are not possible due to site requirement
  posPools <- Filter(Negate(is.null), posPools)
  ## if length greater than 1, check for overlap
  if(length(posPools) > 1) {
    ## get subset of sf object of remaining possible seeds
    datSfSub <- sf::st_as_sf(dat[dat[,siteId] %in% names(posPools),], coords = xy, crs = crs)
    ## get distance matrix of distances between seeds
    posPoolsDM <- units:::set_units(st_distance(datSfSub, datSfSub), "km")
    ## get cookie area
    a <- pi*(r^2)
    ## get overlap between cookies of each possible seed
    overlap <- apply(posPoolsDM, c(1,2), function(x) getOverlap(d = x, r = r, a = a))
    ## replace diagonals with 0s
    diag(overlap) <- 0
    ## get row sums - this is a count of how many times threshold is met or exceeded
    OCs <- apply(overlap >= threshold, 1, function(x) length(which(x)))
    ## tracker for cookies to dropped
    keepers <- 1:length(OCs)
    ## While loop to prune worst offenders out until all problem cookies removed
    while(T){
      ## get max
      max.o <- which(OCs == max(OCs))
      ## if more than one tied, find which has most overlapping area
      if(length(max.o) > 1){
        ta <- apply(overlap[max.o,], 1, function(x) sum(x))
        ## take value(s) with maximum overlapping area
        max.o <- max.o[which(ta == max(ta))]
      }
      ## drop from overlap
      overlap <- overlap[-max.o,-max.o]
      ## update OCs
      OCs <- apply(overlap >= threshold, 1, function(x) length(which(x)))
      ## add to drop tracker
      keepers <- keepers[-max.o]
      ## break if OCs sum reaches 0
      if(sum(OCs) == 0){
        break
      }
    }
    ## prune to final set of seed
    finalPools <- posPools[keepers]
  } else {
    finalPools <- posPools
  }
  return(finalPools)
}
