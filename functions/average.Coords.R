average.Coords <- function(data, latitudes, longitudes){
  ## Count number of coordinates
  la <- data[,latitudes]
  lo <- data[,longitudes]
  la$count <- apply(la, 1, function(x) length(which(!is.na(x))))
  lo$count <- apply(lo, 1, function(x) length(which(!is.na(x))))
  ## Get average latitude
  averageLatitude <- sapply(1:nrow(la), function(x){
    sum(la[x,latitudes][which(!is.na(la[x,latitudes]))])/la[x,"count"]
  })
  ## Get average longitude
  averageLongitude <- sapply(1:nrow(lo), function(x){
    sum(lo[x,longitudes][which(!is.na(lo[x,longitudes]))])/lo[x,"count"]
  })
  ## Add to data
  data <- cbind(data, averageLatitude, averageLongitude)
  return(data)
}
