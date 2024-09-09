mass.mlm.on.summary <- function(input.dir, input.pre, output.dir, output.pre, vars, vars.values, mode = "median", unit = "diff", data.columns = c("Brachiopoda","Bivalvia")){
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".csv")
  output.dir.mlm <- paste0(output.dir, "/", output.pre, "_models.Rds")
  output.dir <- paste0(output.dir, "/", output.pre, ".csv")
  output.mat <- expand.grid(vars.values)
  ## create output matrix
  data.mat <- matrix(NA, ncol = 5, nrow = nrow(output.mat))
  colnames(data.mat) <- c("n.samples", "n.timeBins", "n.subregions", "avg.obs.per.timeBin", "avg.obs.per.subregion")
  ## create output list
  output.list <- lapply(1:nrow(output.mat), function(all) NA)
  ## populate output vectors
  for(i in 1:nrow(output.mat)){
    ## Read in data
    data <- suppressWarnings(tryCatch(read.csv(input.dirs[i], row.names = 1, stringsAsFactors = T), error = function(e){}))
    ## if null, log entries as NA
    if(is.null(data)){
      next
      ## If not null, report number of observations
    } else {
      ## set reference category for full model
      data[,"sampLith"] <- relevel(data[,"sampLith"], ref = "mix")
      data[,"sampEnv"] <- relevel(data[,"sampEnv"], ref = "mix")
      data[,"sampReef"] <- relevel(data[,"sampReef"], ref = "mix")
      data.mat[i,1] <- nrow(data)
      data.mat[i,2] <- length(unique(data[,"times"]))
      data.mat[i,3] <- length(unique(data[,"source.subregion.ID"]))
      data.mat[i,4] <- nrow(data)/(length(unique(data[,"times"])))
      data.mat[i,5] <- nrow(data)/(length(unique(data[,"source.subregion.ID"])))
      medMat <- data.frame(matrix(NA, ncol = 2, nrow = length(unique(data[,"source.subregion.ID"]))))
      times <- c()
      for(t in unique(data[,"times"])){
        times <- c(times,rep(t,length(unique(data[which(data[,"times"] == t),"source.subregion.ID"]))))
      }
      medMat <- cbind(times, unique(data[,"source.subregion.ID"]),medMat)
      colnames(medMat) <- colnames(data)
      if(mode == "median"){
        if(unit == "diff"){
          for(t in 1:nrow(medMat)){
            ## get data
            subregion <- data[which(data[,"source.subregion.ID"] == medMat[t,"source.subregion.ID"]),data.columns]
            ## get differences
            diffs <- apply(subregion, 1, function(x) diff(x))
            ## get ID of median
            med.id <- which(diffs == median(diffs))
            ## if length 0
            if(length(med.id) == 0){
              ## get samples either side of quantile, and pick one
              quants <- sort(diffs)[c(length(diffs)/2,length(diffs)/2+1)]
              ## update med.id
              med.id <- which(diffs == sample(quants,1))
            }
            ## if length is more than 1
            if(length(med.id) > 1){
              med.id <- sample(med.id, 1)
            }
            ## get row med.id
            medMat[t,"Brachiopoda"] <- subregion[med.id,"Brachiopoda"]
            medMat[t,"Bivalvia"] <- subregion[med.id,"Bivalvia"]
          }
        } else {
          if(any(unit == data.columns)){
            for(t in 1:nrow(medMat)){
              ## get data
              subregion <- data[which(data[,"source.subregion.ID"] == medMat[t,"source.subregion.ID"]),data.columns]
              ## get data
              values <- subregion[,unit]
              ## get median.id
              med.id <- which(subregion[,unit] == median(subregion[,unit]))
              ## if length 0
              if(length(med.id) == 0){
                ## get samples either side of quantile, and pick one
                quants <- sort(values)[c(length(values)/2,length(values)/2+1)]
                ## update med.id
                med.id <- which(subregion[,unit] == sample(quants,1))
              }
              ## if length is more than 1
              if(length(med.id) > 1){
                med.id <- sample(med.id, 1)
              }
              ## get row med.id
              medMat[t,"Brachiopoda"] <- subregion[med.id,"Brachiopoda"]
              medMat[t,"Bivalvia"] <- subregion[med.id,"Bivalvia"]
            }
          } else {
            stop("check unit is 'diff' or the name of one of the data columns")
          }
        }
      } else {
        if(mode == "min"){
          if(unit == "diff"){
            for(t in 1:nrow(medMat)){
              ## get data
              subregion <- data[which(data[,"source.subregion.ID"] == medMat[t,"source.subregion.ID"]),data.columns]
              ## get differences
              diffs <- apply(subregion, 1, function(x) diff(x))
              ## get ID of min
              min.id <- which(abs(diffs) == min(abs(diffs)))
              ## if length is more than 1
              if(length(min.id) > 1){
                min.id <- sample(min.id, 1)
              }
              ## get row min.id
              medMat[t,"Brachiopoda"] <- subregion[min.id,"Brachiopoda"]
              medMat[t,"Bivalvia"] <- subregion[min.id,"Bivalvia"]
            }
          } else {
            stop("change argument 'unit' to 'diff'")
          }
        } else {
          stop("check argument 'mode' is 'median' or 'min'")
        }
      }
      mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + sampLith + sampEnv + sampReef + sampLat + (1|times), data = data), error = function(e){}))
      if(is.null(mlm)){
        next
      } else {
        ## Add MLM to list
        output.list[[i]] <- mlm
      }
    }
  }
  output.mat <- cbind(output.mat, data.mat)
  output.mat <- output.mat[which(!apply(output.mat, 1, function(x) any(is.na(x)))),]
  names(output.list) <- varStrings
  output.list <- output.list[!is.na(output.list)]
  write.csv(output.mat, output.dir)
  saveRDS(output.list, output.dir.mlm)
}
