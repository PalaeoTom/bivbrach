cookie <- function(dat, seed, xy, allPools, weight, coords, crs, output) {
  pool <- allPools[seed][[1]]
  if (weight) {
    datSf <- sf::st_as_sf(coords, coords = xy, crs = crs)
    pool <- pool[!pool == seed]
    poolBool <- coords[, "id"] %in% pool
    poolPts <- datSf[poolBool, ]
    seedRow <- which(coords[, "id"] == seed)[1]
    seedPt <- datSf[seedRow, ]
    gcdists <- sf::st_distance(poolPts, seedPt)
    wts <- sapply(gcdists, function(x) x^(-2))
    samplIds <- c(seed, sample(sample(pool), nSite -
                                 1, prob = wts, replace = FALSE))
  }
  else {
    samplIds <- sample(sample(pool), nSite, replace = FALSE)
  }
  coordRows <- match(samplIds, coords$id)
  coordLocs <- coords[coordRows, xy]
  if (output == "full") {
    x <- xy[1]
    y <- xy[2]
    sampPtStrg <- paste(coordLocs[, x], coordLocs[,
                                                  y], sep = "/")
    datPtStrg <- paste(dat[, x], dat[, y], sep = "/")
    inSamp <- match(datPtStrg, sampPtStrg)
    out <- dat[!is.na(inSamp), ]
  }
  else {
    if (output == "locs") {
      out <- coordLocs
    }
    else {
      stop("output argument must be one of c('full', 'locs')")
    }
  }
  return(out)
}
