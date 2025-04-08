## 1.7 Geocoding

## Load libraries
packages <- c("stringr", "dismo", "terra", "countrycode", "gptr")
if(length(packages[!packages %in% installed.packages()[,"Package"]]) > 0){
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}
library(stringr)
library(dismo)
library(terra)
library(countrycode)
library(gptr)

##  Clean directory
rm(list = ls())

## Read in updated databases
PBDB <- readRDS("data/PBDB/PBDB.Rds")
NMS <- readRDS("data/museum/NMS_1_6_1.Rds")
AMNH <- readRDS("data/museum/AMNH_1_6_1.Rds")
Peabody <- readRDS("data/museum/Peabody_1_6_1.Rds")
GBIF <- readRDS("data/GBIF/GBIF_1_6_1.Rds")

#### Get occurrences needing geocoding ####
##### NMS ######
# all need doing. No additional locality information
NMS.loc <- NMS$locality
geocode_NMS <- unique(NMS.loc)
NMS_TBC <- NMS
rm(NMS)

## Check for any empty strings - drop record if so
#View(data.frame(table(geocode_NMS)))
# None!

##### Peabody #####
## Add uncertainty column - default is 0
Peabody$uncertaintyInMeters <- 0

#### Some latitude and longitudes have line breaks
double_lat <- which(str_detect(Peabody[,"latitude"], "\n"))
double_long <- which(str_detect(Peabody[,"longitude"], "\n"))
doubles <- unique(double_lat, double_long)

## get values
dLats <- Peabody[doubles,"latitude"]
dLongs <- Peabody[doubles,"longitude"]

## convert to double
Peabody$latitude <- as.numeric(Peabody$latitude)
Peabody$longitude <- as.numeric(Peabody$longitude)

## Split and get uncertainty
dLats <- as.data.frame(t(data.frame(str_split(dLats, pattern = "\n"))))
rownames(dLats) <- NULL
colnames(dLats) <- c("lat1", "lat2")

dLongs <- as.data.frame(t(data.frame(str_split(dLongs, pattern = "\n"))))
rownames(dLongs) <- NULL
colnames(dLongs) <- c("long1", "long2")

## detect duplicates and clean
dLats[which(apply(dLats, 1, function(x) x[1] == x[2])),2] <- ""
dLongs[which(apply(dLongs, 1, function(x) x[1] == x[2])),2] <- ""

## convert to double and get mean
dLats[,1] <- as.double(dLats[,1])
dLats[,2] <- as.double(dLats[,2])
dLongs[,1] <- as.double(dLongs[,1])
dLongs[,2] <- as.double(dLongs[,2])

## Get average
dLats$avgLat <- NA
dLongs$avgLong <- NA
for(a in 1:nrow(dLats)){
  if(all(!is.na(dLats[a,c(1,2)]))){
    dLats[a,"avgLat"] <- mean(c(dLats[a,"lat1"], dLats[a,"lat2"]))
  } else {
    dLats[a,"avgLat"] <- dLats[a,"lat1"]
  }
}
for(a in 1:nrow(dLongs)){
  if(all(!is.na(dLongs[a,c(1,2)]))){
    dLongs[a,"avgLong"] <- mean(c(dLongs[a,"long1"], dLongs[a,"long2"]))
  } else {
    dLongs[a,"avgLong"] <- dLongs[a,"long1"]
  }
}

## Combine
d <- cbind(dLats, dLongs)

## Calculate uncertainty as radius of circle
d$uncertaintyInMeters <- ""

## Set uncertain as half greatest range in lat/long (i.e., radius of smallest circle that encompasses both values)
for(r in 1:nrow(d)){
  if(length(which(is.na(d[r,c(1,2,4,5)])))==1){
    ## if lat has 1 NA,
    if((length(which(is.na(d[r,c(1,2)])))==1)){
      ## construct matrix
      x <- matrix(data = c(d[r,4],d[r,1]), ncol = 2)
      y <- matrix(data = c(d[r,5],d[r,1]), ncol = 2)
      ## Should be distance
      d[r, "uncertaintyInMeters"] <- round(distance(x, y, lonlat = T)/2, digits = 2)
    }
    ## if long has 1 NA, lat must be different
    if((length(which(is.na(d[r,c(4,5)])))==1)){
      ## construct matrix
      x <- matrix(data = c(d[r,4],d[r,1]), ncol = 2)
      y <- matrix(data = c(d[r,4],d[r,2]), ncol = 2)
      ## Should be distance
      d[r, "uncertaintyInMeters"] <- round(distance(x, y, lonlat = T)/2, digits = 2)
    }
    if(all(!is.na(d[r,c(1,2,4,5)]))){
      ## construct matrix
      x <- matrix(data = c(d[r,4],d[r,1]), ncol = 2)
      y <- matrix(data = c(d[r,4],d[r,5],d[r,5],d[r,2],d[r,1],d[r,2]), ncol = 2)
      ## Should be distance
      d[r, "uncertaintyInMeters"] <- round(max(distance(x, y, lonlat = T))/2, digits = 2)
    }
  }
}

## Return coordinates to Peabody dataset
Peabody[doubles,"latitude"] <- d[,"avgLat"]
Peabody[doubles,"longitude"] <- d[,"avgLong"]
Peabody[doubles,"uncertaintyInMeters"] <- d[,"uncertaintyInMeters"]

## check for weird coords - scheme is EPSG:4326
## Initialise vector for droppers
Peabody_drop <-c()

## Any coordinates outside expected range
if(any(Peabody$latitude[!is.na(Peabody$latitude)] > 180)){
  Peabody_drop <- c(Peabody_drop,which((Peabody$latitude[!is.na(Peabody$latitude)]) > 180))
}
if(any(Peabody$latitude[!is.na(Peabody$latitude)] < -180)){
  Peabody_drop <- c(Peabody_drop,which((Peabody$latitude[!is.na(Peabody$latitude)]) < -180))
}
if(any(Peabody$longitude[!is.na(Peabody$longitude)] > 180)){
  Peabody_drop <- c(Peabody_drop,which((Peabody$longitude[!is.na(Peabody$longitude)]) > 180))
}
if(any(Peabody$longitude[!is.na(Peabody$longitude)] < -180)){
  Peabody_drop <- c(Peabody_drop,which((Peabody$longitude[!is.na(Peabody$longitude)]) < -180))
}

## Any coordinates with unaccpetable uncertainty
Peabody_drop <- c(Peabody_drop, which(Peabody[,"uncertaintyInMeters"] > 10000))

## get unique
Peabody_drop <- unique(Peabody_drop)

## Remove droppers from dataset
Peabody_TBC <- Peabody[Peabody_drop,]
Peabody <- Peabody[-Peabody_drop,]

## Clean dataset
Peabody_TBC[,"latitude"] <- NA
Peabody_TBC[,"longitude"] <- NA
Peabody_TBC[,"uncertaintyInMeters"] <- NA

## get geocode vector
Peabody.loc <- Peabody_TBC[,"locality"]
dcs <- Peabody_TBC[,"districtCountyShire"]

## for each loc, check for district info. Add if present
for(l in 1:length(Peabody.loc)){
  if(!dcs[l]==""){
    Peabody.loc[l] <- str_flatten(c(Peabody.loc[l], dcs[l]), collapse = ", ")
  }
}

## finalise
geocode_Peabody <- unique(Peabody.loc)

## Check for gaps or unknown
#View(data.frame(table(geocode_Peabody)))
## None!

##### AMNH #####
## Add uncertainty column
AMNH$uncertaintyInMeters <- 0

## Manually inspect coordinates - only NAs to drop. Already type double.
#View(data.frame(table(AMNH$latitudeDecimal)))
#View(data.frame(table(AMNH$longitudeDecimal)))

## Get those missing coords
AMNH_drop <- which(is.na(AMNH$latitudeDecimal))
AMNH_drop <- c(AMNH_drop, which(is.na(AMNH$longitudeDecimal)))

## Any coordinates outside expected range
if(any(AMNH$latitudeDecimal[!is.na(AMNH$latitudeDecimal)] > 180)){
  AMNH_drop <- c(AMNH_drop,which((AMNH$latitudeDecimal[!is.na(AMNH$latitudeDecimal)]) > 180))
}
if(any(AMNH$latitudeDecimal[!is.na(AMNH$latitudeDecimal)] < -180)){
  AMNH_drop <- c(AMNH_drop,which((AMNH$latitudeDecimal[!is.na(AMNH$latitudeDecimal)]) < -180))
}
if(any(AMNH$longitudeDecimal[!is.na(AMNH$longitudeDecimal)] > 180)){
  AMNH_drop <- c(AMNH_drop,which((AMNH$longitudeDecimal[!is.na(AMNH$longitudeDecimal)]) > 180))
}
if(any(AMNH$longitudeDecimal[!is.na(AMNH$longitudeDecimal)] < -180)){
  AMNH_drop <- c(AMNH_drop,which((AMNH$longitudeDecimal[!is.na(AMNH$longitudeDecimal)]) < -180))
}

## drop to unique entries
AMNH_drop <- unique(AMNH_drop)

## Separate those entries to be geocoded
AMNH_TBC <- AMNH[AMNH_drop,]
AMNH <- AMNH[-AMNH_drop,]

## Clean TBC
AMNH_TBC$latitudeDecimal <- NA
AMNH_TBC$longitudeDecimal <- NA
AMNH_TBC$uncertaintyInMeters <- NA

## Isolate geographic location vectors
## Precise location no good
#View(data.frame(table(AMNH_TBC$preciseLocation)))
country <- AMNH_TBC$country
state <- AMNH_TBC$state
county <- AMNH_TBC$county
township <- AMNH_TBC$township
AMNH.loc <- AMNH_TBC$locality

## for each loc, check for info, then add
for(l in 1:length(AMNH.loc)){
  if(!township[l]==""){
    AMNH.loc[l] <- str_flatten(c(AMNH.loc[l], township[l]), collapse = ", ")
  }
  if(!county[l]==""){
    AMNH.loc[l] <- str_flatten(c(AMNH.loc[l], county[l]), collapse = ", ")
  }
  if(!state[l]==""){
    AMNH.loc[l] <- str_flatten(c(AMNH.loc[l], state[l]), collapse = ", ")
  }
  if(!country[l]==""){
    AMNH.loc[l] <- str_flatten(c(AMNH.loc[l], country[l]), collapse = ", ")
  }
}

## finalise
geocode_AMNH <- unique(AMNH.loc)

## Check for gaps
#View(data.frame(table(geocode_AMNH)))
# None!

##### GBIF #####
## Already has uncertainty

## Get missing entries
GBIF_drop <- which(is.na(GBIF$decimalLatitude))
GBIF_drop <- c(GBIF_drop, which(is.na(GBIF$decimalLongitude)))
GBIF_drop <- c(GBIF_drop, which(GBIF$coordinateUncertaintyInMeters>10000))

## Any coordinates outside expected range
if(any(GBIF$decimalLatitude[!is.na(GBIF$decimalLatitude)] > 180)){
  GBIF_drop <- c(GBIF_drop,which((GBIF$decimalLatitude[!is.na(GBIF$decimalLatitude)]) > 180))
}
if(any(GBIF$decimalLatitude[!is.na(GBIF$decimalLatitude)] < -180)){
  GBIF_drop <- c(GBIF_drop,which((GBIF$decimalLatitude[!is.na(GBIF$decimalLatitude)]) < -180))
}
if(any(GBIF$decimalLongitude[!is.na(GBIF$decimalLongitude)] > 180)){
  GBIF_drop <- c(GBIF_drop,which((GBIF$decimalLongitude[!is.na(GBIF$decimalLongitude)]) > 180))
}
if(any(GBIF$decimalLongitude[!is.na(GBIF$decimalLongitude)] < -180)){
  GBIF_drop <- c(GBIF_drop,which((GBIF$decimalLongitude[!is.na(GBIF$decimalLongitude)]) < -180))
}

## Refine to unique
GBIF_drop <- unique(GBIF_drop)

## Separate those entries to be geocoded
GBIF_TBC <- GBIF[GBIF_drop,]
GBIF <- GBIF[-GBIF_drop,]

## Clean TBC
GBIF_TBC$decimalLatitude <- NA
GBIF_TBC$decimalLongitude <- NA
GBIF_TBC$coordinateUncertaintyInMeters <- NA

## Isolate geographic location vectors
## Precise location no good
country <- GBIF_TBC$ISO3
SP <- GBIF_TBC$stateProvince
county <- GBIF_TBC$county
munic <- GBIF_TBC$municipality
HG <- GBIF_TBC$higherGeography
GBIF.loc <- GBIF_TBC$locality
vloc <- GBIF_TBC$verbatimLocality

## Convert ISO3 codes to country
country <- countrycode(country, origin = "iso3c", destination = "country.name.en")
country[is.na(country)] <- ""

## Combine locality vectors
sum(table(GBIF.loc[-which(GBIF.loc=="")]))
sum(table(vloc[-which(vloc=="")]))

## GBIF.loc is primary. Use vloc to fill in gap.
checkers <- which(GBIF.loc=="")
fillers <- which(!vloc=="")
if(any(fillers %in% checkers)){
  GBIF.loc[fillers[which(fillers %in% checkers)]] <- vloc[fillers[which(fillers %in% checkers)]]
}

## for each GBIF.loc, check for info, then add
for(l in 1:length(GBIF.loc)){
  if(!munic[l]==""){
    GBIF.loc[l] <- str_flatten(c(GBIF.loc[l], munic[l]), collapse = ", ")
  }
  if(!county[l]==""){
    GBIF.loc[l] <- str_flatten(c(GBIF.loc[l], county[l]), collapse = ", ")
  }
  if(!SP[l]==""){
    GBIF.loc[l] <- str_flatten(c(GBIF.loc[l], SP[l]), collapse = ", ")
  }
  if(!country[l]==""){
    GBIF.loc[l] <- str_flatten(c(GBIF.loc[l], country[l]), collapse = ", ")
  }
  if(!HG[l]==""){
    GBIF.loc[l] <- str_flatten(c(GBIF.loc[l], HG[l]), collapse = ", ")
  }
}

## Remove ", " from front
GBIF.loc <- str_replace(GBIF.loc, "^, ", "")

## check for gaps
#View(data.frame(table(GBIF.loc)))

## Alas, some have slipped through the cracks
droppers <- c()
droppers <- c(droppers, which(GBIF.loc==""))
droppers <- c(droppers, which(GBIF.loc=="?, ?"))
droppers <- c(droppers, which(GBIF.loc=="9344"))
droppers <- c(droppers, which(GBIF.loc=="Loc.?"))
droppers <- c(droppers, which(GBIF.loc=="Localité inconnue"))
droppers <- c(droppers, which(GBIF.loc=="locality not specified"))
droppers <- c(droppers, which(GBIF.loc=="no data, Earth"))
droppers <- c(droppers, which(GBIF.loc=="no locality data"))
droppers <- c(droppers, which(GBIF.loc=="Unrecorded"))
droppers <- c(droppers, which(GBIF.loc=="unknown locality, Earth, Earth"))
droppers <- unique(droppers)
GBIF_TBC <- GBIF_TBC[-droppers,]
GBIF.loc <- GBIF.loc[-droppers]

## finalise
geocode_GBIF <- unique(GBIF.loc)

#### Cleaning locality strings using ChatGPT (!) ####
## Doesn't work particularly well - messes up special characters, quality of formatting seems to diminish at higher throughputs
## Set API key
gptKey <- "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"
Sys.setenv(OPENAI_API_KEY = gptKey)

## Define instruction string
instruct <- "Re-write this description of a geographic location for clarity, removing all references to rock beds, stratigraphic units, and geological time units. Do not return anything other than the updated locality string. Description to be re-written: "

## Combine instruction string with each locality string
## AMNH
AMNH_key <- data.frame(geocode_AMNH)
colnames(AMNH_key) <- "original"
AMNH_key$gpt <- ""
for(r in 1:nrow(AMNH_key)){
  AMNH_key[r,"gpt"] <- str_flatten(c(instruct, AMNH_key[r,"original"]), collapse = "")
}
AMNH_key$response <- ""

## GBIF
GBIF_key <- data.frame(geocode_GBIF)
colnames(GBIF_key) <- "original"
GBIF_key$gpt <- ""
for(r in 1:nrow(GBIF_key)){
  GBIF_key[r,"gpt"] <- str_flatten(c(instruct, GBIF_key[r,"original"]), collapse = "")
}
GBIF_key$response <- ""

## Peabody
Peabody_key <- data.frame(geocode_Peabody)
colnames(Peabody_key) <- "original"
Peabody_key$gpt <- ""
for(r in 1:nrow(Peabody_key)){
  Peabody_key[r,"gpt"] <- str_flatten(c(instruct, Peabody_key[r,"original"]), collapse = "")
}
Peabody_key$response <- ""

## NMS
NMS_key <- data.frame(geocode_NMS)
colnames(NMS_key) <- "original"
NMS_key$gpt <- ""
for(r in 1:nrow(NMS_key)){
  NMS_key[r,"gpt"] <- str_flatten(c(instruct, NMS_key[r,"original"]), collapse = "")
}
NMS_key$response <- ""

## Start with Peabody
for(r in 1:nrow(Peabody_key)){
  skip_to_next <- F
  tryCatch(Peabody_key[r,"response"] <- get_response(Peabody_key[r, "gpt"], model = "gpt-4o")$choices$message$content, error = function(e) {skip_to_next <<-TRUE})
  if(skip_to_next){next}
}

## Start with NMS
for(r in 1:nrow(NMS_key)){
  skip_to_next <- F
  tryCatch(NMS_key[r,"response"] <- get_response(NMS_key[r, "gpt"], model = "gpt-4o")$choices$message$content, error = function(e) {skip_to_next <<-TRUE})
  if(skip_to_next){next}
}

## Start with AMNH
for(r in 1:nrow(AMNH_key)){
  skip_to_next <- F
  tryCatch(AMNH_key[r,"response"] <- get_response(AMNH_key[r, "gpt"], model = "gpt-4o")$choices$message$content, error = function(e) {skip_to_next <<-TRUE})
  if(skip_to_next){next}
}

## Start with GBIF
for(r in 1:nrow(GBIF_key)){
  skip_to_next <- F
  tryCatch(GBIF_key[r,"response"] <- get_response(GBIF_key[r, "gpt"], model = "gpt-4o")$choices$message$content, error = function(e) {skip_to_next <<-TRUE})
  if(skip_to_next){next}
}

## Export
saveRDS(GBIF_key, file = "data/GBIF/GBIF_GPT_output.Rds")
saveRDS(AMNH_key, file = "data/museum/AMNH_GPT_output.Rds")
saveRDS(NMS_key, file = "data/museum/NMS_GPT_output.Rds")
saveRDS(Peabody_key, file = "data/museum/Peabody_GPT_output.Rds")

##### Tidying up chatGPT outputs ####
### Read them in
GBIF_key <- readRDS("data/GBIF/GBIF_GPT_output.Rds")
Peabody_key <- readRDS("data/museum/Peabody_GPT_output.Rds")
AMNH_key <- readRDS("data/museum/AMNH_GPT_output.Rds")
NMS_key <- readRDS("data/museum/NMS_GPT_output.Rds")

## Find gaps - use original
GBIF_geostring <- GBIF_key[,"response"]
GBIF_geostring[which(GBIF_geostring == "")] <- GBIF_key[which(GBIF_geostring == ""),"original"]

AMNH_geostring <- AMNH_key[,"response"]
AMNH_geostring[which(AMNH_geostring == "")] <- AMNH_key[which(AMNH_geostring == ""),"original"]

NMS_geostring <- NMS_key[,"response"]
NMS_geostring[which(NMS_geostring == "")] <- NMS_key[which(NMS_geostring == ""),"original"]

Peabody_geostring <- Peabody_key[,"response"]
Peabody_geostring[which(Peabody_geostring == "")] <- Peabody_key[which(Peabody_geostring == ""),"original"]


#### Resume from here ####


## Find

View(data.frame(GBIF_geostring))
GBIF_geostring[which(grepl("[^ -~]", GBIF_geostring))]

## Find new entries with special characters - drop and use original string (gptr::get_response returns rubbish characters when dealing with accents)
GBIF_geostring[which(grepl("[^ -~]", GBIF_geostring))] <- GBIF_key[which(grepl("[^ -~]", GBIF_geostring)),"original"]
Peabody_geostring[which(grepl("[^ -~]", Peabody_geostring))] <- Peabody_key[which(grepl("[^ -~]", Peabody_geostring)),"original"]
NMS_geostring[which(grepl("[^ -~]", NMS_geostring))] <- NMS_key[which(grepl("[^ -~]", NMS_geostring)),"original"]
AMNH_geostring[which(grepl("[^ -~]", AMNH_geostring))] <- AMNH_key[which(grepl("[^ -~]", AMNH_geostring)),"original"]

#### Georeferencing GBIF ####
## Register google maps API keys
gMAPIKey <- "AIzaSyAeUFGhS8Inob5ByMIPTokWg076qmStEV0"

## Run test with dismo - works!
test_dismo <- dismo::geocode(NMS_geostring[1:10], oneRecord = F, geocode_key = gMAPIKey)
test_dismo_2 <- dismo::geocode(NMS_geostring[1:10], oneRecord = T, geocode_key = gMAPIKey)

## Run final geocoding and export
NMS_TBC_coords <- dismo::geocode(NMS_geostring, oneRecord = T, geocode_key = gMAPIKey)
AMNH_TBC_coords <- dismo::geocode(AMNH_geostring, oneRecord = T, geocode_key = gMAPIKey)
Peabody_TBC_coords <- dismo::geocode(Peabody_geostring, oneRecord = T, geocode_key = gMAPIKey)
GBIF_TBC_coords <- dismo::geocode(GBIF_geostring, oneRecord = T, geocode_key = gMAPIKey)

## Export
saveRDS(NMS_TBC_coords, "data/museum/NMS_dismo_coords.Rds")
saveRDS(AMNH_TBC_coords, "data/museum/AMNH_dismo_coords.Rds")
saveRDS(Peabody_TBC_coords, "data/museum/Peabody_dismo_coords.Rds")
saveRDS(GBIF_TBC_coords, "data/GBIF/GBIF_dismo_coords.Rds")
