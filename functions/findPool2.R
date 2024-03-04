findPool2 <- function (seedRow, datSf, sites, xy, r, crs = "epsg:4326"){
  seedpt <- datSf[seedRow, ]
  buf <- sf::st_buffer(seedpt, dist = r)
  if (crs != "epsg:4326") {
    buf <- sf::st_transform(buf, crs = "epsg:4326")
    datSf <- sf::st_transform(datSf, crs = "epsg:4326")
  }
  bufWrap <- sf::st_wrap_dateline(buf, options = c("WRAPDATELINE=YES"))
  poolBool <- sf::st_intersects(datSf, bufWrap, sparse = FALSE)
  pool <- sites[poolBool]
  return(pool)
}
