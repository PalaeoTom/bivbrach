cleanCoordinates <- function(GBIF_data, ISO.codes, capitalBuffer = 1000, countryBuffer = 25000){
  ## Now to check quality of coordinate data
  split.errors <- str_split(GBIF_data$issue, pattern = ";")
  fatal.issues <- c("COORDINATE_INVALID", "COORDINATE_OUT_OF_RANGE", "COORDINATE_REPROJECTION_FAILED", "COORDINATE_REPROJECTION_SUSPICIOUS", "COORDINATE_UNCERTAINTY_METERS_INVALID",
                    "COUNTRY_COORDINATE_MISMATCH", "GEODETIC_DATUM_INVALID", "PRESUMED_NEGATIVE_LATITUDE", "PRESUMED_NEGATIVE_LONGITUDE", "PRESUMED_SWAPPED_COORDINATES",
                    "ZERO_COORDINATE")
  ## pass over list of split errors, check for fatal errors
  nulls <- c()
  for(x in 1:length(split.errors)){
    if(any(fatal.issues %in% split.errors[[x]])){
      nulls <- c(nulls, x)
    }
  }
  if(length(nulls) > 0){
    GBIF_data[nulls, c("decimalLongitude", "decimalLatitude", "coordinateUncertaintyInMeters")] <- NA
  }
  ## add 3 digit ISO codes
  GBIF_data <- merge(GBIF_data, ISO.codes, by.x = "countryCode", by.y = "ISO2", all.x = T)
  ## Check for intersect between cc_rows and flagged vector populated by cc cleaner functions
  f1 <- CoordinateCleaner::cc_val(GBIF_data, lat = "decimalLatitude", lon = "decimalLongitude", value = "flagged")
  ## Create a vector of row IDs to compare against cc_rows
  flagged <- which(!f1)
  # equal latitude and longitudes
  f2 <- CoordinateCleaner::cc_equ(GBIF_data, lat = "decimalLatitude", lon = "decimalLongitude", value = "flagged")
  flagged <- c(flagged,which(!f2))
  # country centroids
  f3 <- CoordinateCleaner::cc_cen(GBIF_data, lat = "decimalLatitude", lon = "decimalLongitude", value = "flagged")
  flagged <- c(flagged,which(!f3))
  # institution coordinates
  f4 <- CoordinateCleaner::cc_inst(GBIF_data, lat = "decimalLatitude", lon = "decimalLongitude", value = "flagged")
  flagged <- c(flagged,which(!f4))
  # GBIF headquarters
  f5 <- CoordinateCleaner::cc_gbif(GBIF_data, lat = "decimalLatitude", lon = "decimalLongitude", value = "flagged")
  flagged <- c(flagged,which(!f5))
  # country capitals (within 1km)
  f6 <- CoordinateCleaner::cc_cap(GBIF_data, lat = "decimalLatitude", lon = "decimalLongitude", value = "flagged", buffer = capitalBuffer)
  flagged <- c(flagged,which(!f6))
  # country lookup (25km buffer)
  f7 <- CoordinateCleaner::cc_coun(GBIF_data, lat = "decimalLatitude", lon = "decimalLongitude", value = "flagged", iso3 = "ISO3", buffer = countryBuffer)
  flagged <- c(flagged,which(!f7))
  ## refine to unique rows
  flagged <- unique(flagged)
  ## now we have cc_rows that are problematic.
  if(length(flagged) > 0){
    GBIF_data[flagged, c("decimalLongitude", "decimalLatitude", "coordinateUncertaintyInMeters")] <- NA
  }
  return(GBIF_data)
}
