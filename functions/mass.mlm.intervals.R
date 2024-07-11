mass.mlm.intervals <- function(input.dir, input.pre, output.dir, output.pre, vars, vars.values, time.cutoffs){
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".csv")
  output.mat <- expand.grid(vars.values)
  ## create output matrix
  data.mat <- matrix(NA, ncol = 15, nrow = nrow(output.mat))
  colnames(data.mat) <- c("n.samples", "n.timeBins", "n.subregions", "avg.obs.per.timeBin", "avg.obs.per.subregion", "intercept", "intercept.std.error", "intercept.df", "intercept.t.value", "intercept.p.value", "slope", "slope.std.error", "slope.df", "slope.t.value", "slope.p.value")
  ## create output list
  output.list <- lapply(1:nrow(output.mat), function(all) NA)
  ## create nested output object
  data.list <- lapply(1:nrow(time.cutoffs), function(all) mat <- data.mat)
  ## create nested model
  output.model.list <- lapply(1:nrow(time.cutoffs), function(all) mat <- output.list)
  ## populate output vectors
  for(i in 1:nrow(output.mat)){
    ## Read in data
    allData <- suppressWarnings(tryCatch(read.csv(input.dirs[i], row.names = 1), error = function(e){}))
    ## if null, log entries as NA
    if(is.null(allData)){
      next
    } else {
      for(t in 1:nrow(time.cutoffs)){
        data <- allData[intersect(which(allData[,"times"] >= time.cutoffs[t,"top"]),which(allData[,"times"] <= time.cutoffs[t,"bottom"])),]
        data.list[[t]][i,1] <- nrow(data)
        data.list[[t]][i,2] <- length(unique(data[,"times"]))
        data.list[[t]][i,3] <- length(unique(data[,"source.subregion.ID"]))
        data.list[[t]][i,4] <- nrow(data)/(length(unique(data[,"times"])))
        data.list[[t]][i,5] <- nrow(data)/(length(unique(data[,"source.subregion.ID"])))
        mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + (1|times/source.subregion.ID), data = data), error = function(e){}))
        if(is.null(mlm)){
          next
        } else {
          ## Add MLM to list
          output.model.list[[t]][[i]] <- mlm
          ## get coefficients
          coefficients <- coef(summary(mlm))
          data.list[[t]][i,6] <- coefficients[1,1]
          data.list[[t]][i,7] <- coefficients[1,2]
          data.list[[t]][i,8] <- coefficients[1,3]
          data.list[[t]][i,9] <- coefficients[1,4]
          data.list[[t]][i,10] <- coefficients[1,5]
          data.list[[t]][i,11] <- coefficients[2,1]
          data.list[[t]][i,12] <- coefficients[2,2]
          data.list[[t]][i,13] <- coefficients[2,3]
          data.list[[t]][i,14] <- coefficients[2,4]
          data.list[[t]][i,15] <- coefficients[2,5]
        }
      }
    }
  }
  output.list <- lapply(1:length(data.list), function(x) cbind(output.mat, data.list[[x]]))
  output.list <- lapply(1:length(output.list), function(x) output.list[[x]][which(!apply(output.mat, 1, function(x) any(is.na(x)))),])
  output.model.list <- lapply(1:length(output.model.list), function(x){
    out <- output.model.list[[x]]
    names(out) <- varStrings
    out <- out[!is.na(out)]
    return(out)
  })
  ## write each output as csv
  for(s in 1:length(output.list)){
    write.csv(output.list[[s]], paste0(output.dir, "/", output.pre, "_", rownames(time.cutoffs)[s],".csv"))
    saveRDS(output.model.list[[s]], paste0(output.dir, "/", output.pre, "_", rownames(time.cutoffs)[s],"_models.Rds"))
  }
}
