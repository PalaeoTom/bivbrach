findPool2 <- function (seedRow, datSV, sites, xy, r, crs = "epsg:4326"){
  seedpt <- datSV[seedRow, ]
  buf <- terra::buffer(seedpt, width = r)
  if (crs != "epsg:4326") {
    buf <- terra::project(buf, y = "epsg:4326")
    datSV <- terra::project(datSV, y = "epsg:4326")
  }
  poolBool <- terra::extract(buf, datSV)
  pool <- sites[which(!is.na(poolBool[, 2]))]
  return(pool)
}
