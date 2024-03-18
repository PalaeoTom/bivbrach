findSeeds2 <- function(dat, siteId, xy, r, nSite, crs = "epsg:4326", threshold = 1, seeding = NULL){
  if(is.null(seeding)){
    sites <- dat[, siteId]
    datSf <- sf::st_as_sf(dat, coords = xy, crs = crs)
    rad <- units::set_units(r, "km")
    posPools <- lapply(sites, function(s) {
      seedRow <- which(dat[, siteId] == s)[1]
      sPool <- findPool2(seedRow, datSf, sites, xy, rad, crs)
      n <- length(sPool)
      if (n >= nSite)
        sPool
    })
  } else {
    sites <- dat[seeding, siteId]
    datSf <- sf::st_as_sf(dat, coords = xy, crs = crs)
    rad <- units::set_units(r, "km")
    posPools <- lapply(sites, function(s) {
      seedRow <- which(dat[, siteId] == s)[1]
      sPool <- findPool2(seedRow, datSf, dat[,siteId], xy, rad, crs)
      n <- length(sPool)
      if (n >= nSite)
        sPool
    })
  }
  names(posPools) <- sites
  posPools <- Filter(Negate(is.null), posPools)
  if(length(posPools) > 1) {
    datSfSub <- sf::st_as_sf(dat[dat[,siteId] %in% names(posPools),], coords = xy, crs = crs)
    posPoolsDM <- units:::set_units(sf:::st_distance(datSfSub, datSfSub), "km")
    a <- pi*(r^2)
    overlap <- apply(posPoolsDM, c(1,2), function(x) getOverlap(d = x, r = r, a = a))
    diag(overlap) <- 0
    if(threshold > 0) {
      OCs <- apply(overlap >= threshold, 1, function(x) length(which(x)))
    } else {
      OCs <- apply(overlap > threshold, 1, function(x) length(which(x)))
    }
    keepers <- 1:length(OCs)
    if(sum(OCs) > 0){
      while(T){
        max.o <- which(OCs == max(OCs))
        if(length(max.o) > 1){
          ta <- apply(overlap[max.o,], 1, function(x) sum(x))
          max.o <- max.o[sample(which(ta == max(ta)),1)]
        }
        overlap <- overlap[-max.o,-max.o]
        if(threshold > 0) {
          OCs <- apply(overlap >= threshold, 1, function(x) length(which(x)))
        } else {
          OCs <- apply(overlap > threshold, 1, function(x) length(which(x)))
        }
        keepers <- keepers[-max.o]
        if(sum(OCs) == 0){
          break
        }
      }
    }
    finalPools <- posPools[keepers]
  } else {
    finalPools <- posPools
  }
  return(finalPools)
}
