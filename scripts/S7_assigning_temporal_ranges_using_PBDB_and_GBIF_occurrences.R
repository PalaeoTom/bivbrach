###### PBDB gap filling ######
## Load version
GBIF <- readRDS("data/GBIF/GBIF_1_6_1.Rds")

## Get starting point
missing <- which(GBIF$chronostratigraphy == "")
n.usable.pMacrostrat <- nrow(GBIF)-length(missing)
prop.usable.pMacrostrat <- (nrow(GBIF)-length(missing))/nrow(GBIF)

## Cycle through each row, updating to match PBDB
for(i in missing){
  print(i)
  ## get formations
  form <- c(GBIF[i, "formation1"],GBIF[i, "formation2"],GBIF[i, "formation3"],GBIF[i, "formation4"])
  ## if all gaps, skip
  if(all(form == "")){
    next
  } else {
    ## if at least one is not blank, cut down
    if(any(form == "")){
      form <- form[-which(form == "")]
    }
    ## if more than one formation, do something
    if(length(form) > 1){
      ## initialise
      chronos.f <- c()
      for(f in form){
        ## Step 1: check within PBDB
        hits <- which(PBDB$formation == f)
        ## If hits has at least one number, proceed
        if(length(hits)>0){
          ## get unique intervals
          chronos <- c(PBDB[hits,"early_interval"],PBDB[hits,"late_interval"])
          chronos <- unique(chronos)
          ## drop NAs if any
          if(any(is.na(chronos))){
            chronos <- chronos[!is.na(chronos)]
          }
          ## record
          chronos.f <- c(chronos.f, chronos)
        } else {
          next
        }
      }
      if(length(chronos.f) > 0){
        ## Get unique
        chronos.f <- unique(chronos.f)
        ## If 2 or more unique elements, flatten into one.
        if(length(chronos.f) > 1){
          chronos.f <- str_flatten(chronos.f, collapse = ",")
          GBIF[i,"chronostratigraphy"] <- chronos.f
          GBIF[i,"chronostratigraphySource"] <- "PBDB"
        } else {
          GBIF[i,"chronostratigraphy"] <- chronos.f
          GBIF[i,"chronostratigraphySource"] <- "PBDB"
        }
      } else {
        next
      }
    } else {
      ## Step 1: check within PBDB
      hits <- which(PBDB$formation == form)
      ## If hits has at least one number, proceed
      if(length(hits)>0){
        ## get unique intervals
        chronos <- c(PBDB[hits,"early_interval"],PBDB[hits,"late_interval"])
        chronos <- unique(chronos)
        ## drop NAs if any
        if(any(is.na(chronos))){
          chronos <- chronos[!is.na(chronos)]
        }
        ## if length is more than one, combine with commas
        if(length(chronos)>1){
          chronos <- str_flatten(chronos, collapse = ",")
          GBIF[i,"chronostratigraphy"] <- chronos
          GBIF[i,"chronostratigraphySource"] <- "PBDB"
        } else {
          ## only 1 hit - just use that
          GBIF[i,"chronostratigraphy"] <- chronos
          GBIF[i,"chronostratigraphySource"] <- "PBDB"
        }
      } else {
        next
      }
    }
  }
}

## Export updated GBIF
saveRDS(GBIF, file = "data/GBIF/GBIF_1_6_2.Rds")

###### Within dataset ######
## Load latest version
GBIF <- readRDS("data/GBIF/GBIF_1_6_2.Rds")

## Get missing entries
missing <- which(GBIF$chronostratigraphy == "")
n.usable.pPBDB <- nrow(GBIF)-length(missing)
prop.usable.pPBDB <- (nrow(GBIF)-length(missing))/nrow(GBIF)

## Cycle through missing
for(i in missing){
  print(which(missing == i))
  ## get formations
  form <- c(GBIF[i, "formation1"],GBIF[i, "formation2"],GBIF[i, "formation3"],GBIF[i, "formation4"])
  ## if all gaps, skip
  if(all(form == "")){
    next
  } else {
    ## if at least one is not blank, cut down
    if(any(form == "")){
      form <- form[-which(form == "")]
    }
    ## if more than one formation, do something
    if(length(form) > 1){
      ## initialise
      chronos.f <- c()
      for(f in form){
        ## Step 1: check within dataset
        hit.in <- unique(c(which(GBIF$formation1 == f),which(GBIF$formation2 == f),which(GBIF$formation3 == f),which(GBIF$formation4 == f)))
        ## If any hits don't match i, try
        if(any(!hit.in == i)){
          ## get hits
          hits <- hit.in[which(!hit.in == i)]
          ## get chronostratigraphies
          chronos <- unique(GBIF[hits,"chronostratigraphy"])
          ## if all gaps, skip
          if(all(chronos == "")){
            next
          } else {
            ## drop gaps if any
            if(any(chronos == "")){
              chronos <- chronos[-which(chronos == "")]
            }
            ## Check for commas. Split if necessary
            if(any(str_detect(chronos, ","))){
              ## split that which needs to be split and add to output
              chronos <- unique(unlist(str_split(chronos, pattern = ",")))
              chronos.f <- c(chronos.f, chronos)
            } else {
              ## Add to output vector
              chronos.f <- c(chronos.f, chronos)
            }
          }
        } else {
          next
        }
      }
      ## If length of chronos.f above 0, attach
      if(length(chronos.f) > 0){
        chronos.f <- unique(chronos.f)
        ## If length above 1, flatten
        if(length(chronos.f) > 1){
          chronos.f <- str_flatten(chronos.f, collapse = ",")
          GBIF[i,"chronostratigraphy"] <- chronos.f
          GBIF[i,"chronostratigraphySource"] <- "GBIF"
        } else {
          GBIF[i,"chronostratigraphy"] <- chronos.f
          GBIF[i,"chronostratigraphySource"] <- "GBIF"
        }
      } else {
        next
      }
    } else {
      ## Step 1: check within dataset
      hit.in <- unique(c(which(GBIF$formation1 == form),which(GBIF$formation2 == form),which(GBIF$formation3 == form),which(GBIF$formation4 == form)))
      ## If any hits don't match i, try
      if(any(!hit.in == i)){
        ## get hits
        hits <- hit.in[which(!hit.in == i)]
        ## get chronostratigraphies
        chronos <- unique(GBIF[hits,"chronostratigraphy"])
        ## if all gaps, skip
        if(all(chronos == "")){
          next
        } else {
          ## drop gaps if any and retain unique
          if(any(chronos == "")){
            chronos <- chronos[-which(chronos == "")]
          }
          ## Check for commas. Split if necessary
          if(any(str_detect(chronos, ","))){
            chronos <- unique(unlist(str_split(chronos, pattern = ",")))
          }
          ## If length above 1, combine and record
          if(length(chronos)>1){
            chronos <- str_flatten(chronos, ",")
            GBIF[i,"chronostratigraphy"] <- chronos
            GBIF[i,"chronostratigraphySource"] <- "GBIF"
          } else {
            GBIF[i,"chronostratigraphy"] <- chronos
            GBIF[i,"chronostratigraphySource"] <- "GBIF"
          }
        }
      } else {
        next
      }
    }
  }
}
