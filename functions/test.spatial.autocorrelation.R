test.spatial.autocorrelation <- function(model, data, CRS = "EPSG:8857", res = 100000){
  rPrj <- terra::project(x = terra::rast(), y =  CRS,
                         res = res)
  terra::values(rPrj) <- 1:terra::ncell(rPrj)
  llOccs <- terra::vect(data, geom = c("long","lat"), crs = CRS)
  data[, "cell"] <- terra::cells(rPrj, llOccs)[, "cell"]
  data[, c("cellLong", "cellLat")] <- terra::xyFromCell(rPrj, data$cell)
  ## Extract and remove duplicates
  groupLocations <- data[,c(11:13)]
  groupLocations <- groupLocations[!duplicated(groupLocations),]
  ## Calculate residuals
  resmodel <- simulateResiduals(model, re.form = NULL)
  ## Recalculate residuals for each grid cell
  resmodel2 <- recalculateResiduals(resmodel, data$cell, mean)
  ## Now test for spatialautocorrelation
  testSpatialAutocorrelation(resmodel2, x = groupLocations$cellLong, y = groupLocations$cellLat)
}
