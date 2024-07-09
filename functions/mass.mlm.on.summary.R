mass.mlm.on.summary <- function(input.dir, input.pre, output.dir, output.pre, vars, vars.values, mode = "median", unit = "diff", data.columns = c("Brachiopoda","Bivalvia")){
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".csv")
  output.dir <- paste0(output.dir, "/", output.pre, ".csv")
  output.mat <- expand.grid(vars.values)
  ## create output vectors
  n.samples <- c()
  n.timeBins <- c()
  n.subregions <- c()
  avg.obs.per.timeBin <- c()
  avg.obs.per.subregion <- c()
  intercept <- c()
  intercept.std.Error <- c()
  intercept.df <- c()
  intercept.t.value <- c()
  intercept.p.value <- c()
  slope <- c()
  slope.std.Error <- c()
  slope.df <- c()
  slope.t.value <- c()
  slope.p.value <- c()
  ## populate output vectors
  for(i in 1:nrow(output.mat)){
    ## Read in data
    data <- suppressWarnings(tryCatch(read.csv(input.dirs[i], row.names = 1), error = function(e){}))
    ## if null, log entries as NA
    if(is.null(data)){
      n.samples <- c(n.samples, NA)
      n.timeBins <- c(n.timeBins, NA)
      n.subregions <- c(n.subregions, NA)
      avg.obs.per.timeBin <- c(avg.obs.per.timeBin, NA)
      avg.obs.per.subregion <- c(avg.obs.per.subregion, NA)
      intercept <- c(intercept, NA)
      intercept.std.Error <- c(intercept.std.Error, NA)
      intercept.df <- c(intercept.df, NA)
      intercept.t.value <- c(intercept.t.value, NA)
      intercept.p.value <- c(intercept.p.value, NA)
      slope <- c(slope, NA)
      slope.std.Error <- c(slope.std.Error, NA)
      slope.df <- c(slope.df, NA)
      slope.t.value <- c(slope.t.value, NA)
      slope.p.value <- c(slope.p.value, NA)
      ## If not null, report number of observations
    } else {
      ## if data has just one column, transpose
      n.samples <- c(n.samples, nrow(data))
      n.timeBins <- c(n.timeBins, length(unique(data[,"times"])))
      n.subregions <- c(n.subregions, length(unique(data[,"source.subregion.ID"])))
      avg.obs.per.timeBin <- c(avg.obs.per.timeBin, nrow(data)/(length(unique(data[,"times"]))))
      avg.obs.per.subregion <- c(avg.obs.per.subregion, nrow(data)/(length(unique(data[,"source.subregion.ID"]))))
      ## run model - not enough degrees of freedom for random slope, only random intercept
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
      #mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + (1 + Bivalvia|times), data = data), error = function(e){}))
      mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + (1|times), data = medMat), error = function(e){}))
      if(is.null(mlm)){
        intercept <- c(intercept, NA)
        intercept.std.Error <- c(intercept.std.Error, NA)
        intercept.df <- c(intercept.df, NA)
        intercept.t.value <- c(intercept.t.value, NA)
        intercept.p.value <- c(intercept.p.value, NA)
        slope <- c(slope, NA)
        slope.std.Error <- c(slope.std.Error, NA)
        slope.df <- c(slope.df, NA)
        slope.t.value <- c(slope.t.value, NA)
        slope.p.value <- c(slope.p.value, NA)
      } else {
        ## get coefficients
        coefficients <- coef(summary(mlm))
        intercept <- c(intercept, coefficients[1,1])
        intercept.std.Error <- c(intercept.std.Error, coefficients[1,2])
        intercept.df <- c(intercept.df, coefficients[1,3])
        intercept.t.value <- c(intercept.t.value, coefficients[1,4])
        intercept.p.value <- c(intercept.p.value, coefficients[1,5])
        slope <- c(slope, coefficients[2,1])
        slope.std.Error <- c(slope.std.Error, coefficients[2,2])
        slope.df <- c(slope.df, coefficients[2,3])
        slope.t.value <- c(slope.t.value, coefficients[2,4])
        slope.p.value <- c(slope.p.value, coefficients[2,5])
      }
    }
  }
  output.mat <- cbind(output.mat, n.samples, n.timeBins, n.subregions, avg.obs.per.timeBin, avg.obs.per.subregion, intercept, intercept.std.Error,
                      intercept.df, intercept.t.value, intercept.p.value, slope, slope.std.Error,
                      slope.df, slope.t.value, slope.p.value)
  output.mat <- output.mat[which(!apply(output.mat, 1, function(x) any(is.na(x)))),]
  write.csv(output.mat, output.dir)
}
