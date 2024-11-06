import.raw.GBIF <- function(path, retained.columns = NULL, export.dir, export = F, export.name = "new"){
  ## read data into R
  raw_GBIF <- occ_download_import(as.download(path))
  ## prune out non-fossils
  if(any(!raw_GBIF[,"basisOfRecord"] == "FOSSIL_SPECIMEN")){
    d <- raw_GBIF[which(raw_GBIF[,"basisOfRecord"] == "FOSSIL_SPECIMEN"),]
  }
  ## prune out absences
  if(any(!d[,"occurrenceStatus"] == "PRESENT")){
    d <- d[which(d[,"occurrenceStatus"] == "PRESENT"),]
  }
  ## drop Palaeobiology Database entries
  if(any(d[,"publisher"] == "Paleobiology Database")){
    d <- d[-which(d[,"publisher"] == "Paleobiology Database"),]
  }
  ## prune nonessential columns
  if(!is.null(retained.columns)){
    d <- d[,retained.columns]
  }
  if(export){
    saveRDS(d, paste0(out.dir, "/", export.name, ".Rds"))
  }
  return(d)
}
