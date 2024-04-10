findSeeds2 <- function(dat, siteId, xy, r, nSite, crs = "epsg:4326", oThreshold = 1, oType = "area", seeding = NULL){
  if(is.null(seeding)){
    sites <- dat[, siteId]
    datSV <- terra::vect(dat, geom = xy, crs = crs)
    posPools <- lapply(sites, function(s) {
      seedRow <- which(dat[, siteId] == s)[1]
      sPool <- findPool2(seedRow, datSV, sites, xy, r, crs)
      n <- length(sPool)
      if (n >= nSite)
        sPool
    })
  } else {
    sites <- dat[seeding, siteId]
    datSV <- terra::vect(dat, geom = xy, crs = crs)
    posPools <- lapply(sites, function(s) {
      seedRow <- which(dat[, siteId] == s)[1]
      sPool <- findPool2(seedRow, datSV, dat[,siteId], xy, r, crs)
      n <- length(sPool)
      if (n >= nSite)
        sPool
    })
  }
  names(posPools) <- sites
  posPools <- Filter(Negate(is.null), posPools)
  if(length(posPools) > 1) {
    if(oType == "area"){
    datSVSub <- terra::vect(dat[dat[,siteId] %in% names(posPools),], geom = xy, crs = crs)
    posPoolsDM <- terra::distance(datSVSub, datSVSub)
    a <- pi*(r^2)
    overlap <- apply(posPoolsDM, c(1,2), function(x) getOverlap(d = x, r = r, a = a))
    diag(overlap) <- 0
    if(oThreshold > 0) {
      OCs <- apply(overlap >= oThreshold, 1, function(x) length(which(x)))
    } else {
      OCs <- apply(overlap > oThreshold, 1, function(x) length(which(x)))
    }
    } else {
      if(oType == "sites"){
        index <- expand.grid(seq(1,length(posPools),1),seq(1,length(posPools),1))
        ShaSi <- matrix(0, length(posPools), length(posPools))
        nSi <- sapply(posPools, function(x) length(x))
        for (i in 1:nrow(index)){
          ShaSi[index[i,1],index[i,2]] <- length(intersect(posPools[[index[i,1]]],posPools[[index[i,2]]]))
        }
        overlap <- t(sapply(1:nrow(ShaSi), function(x) ShaSi[x,]/nSi[x]))
        diag(overlap) <- 0
        if(oThreshold > 0) {
          OCs <- apply(overlap >= oThreshold, 1, function(x) length(which(x)))
        } else {
          OCs <- apply(overlap > oThreshold, 1, function(x) length(which(x)))
        }
      } else {
        stop("Argument oType needs to be 'area' or 'sites'")
      }
    }
    if(sum(OCs) > 0){
      keepers <- 1:length(OCs)
      while(T){
        max.o <- which(OCs == max(OCs))
        if(length(max.o) > 1){
          ta <- apply(overlap[max.o,], 1, function(x) sum(x))
          max.o <- max.o[sample(which(ta == max(ta)),1)]
        }
        overlap <- overlap[-max.o,-max.o]
        if(oThreshold > 0) {
          OCs <- apply(overlap >= oThreshold, 1, function(x) length(which(x)))
        } else {
          OCs <- apply(overlap > oThreshold, 1, function(x) length(which(x)))
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
