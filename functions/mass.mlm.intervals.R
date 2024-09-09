mass.mlm.intervals <- function(input.dir, input.pre, output.dir, output.pre, vars, vars.values, time.cutoffs){
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".csv")
  output.mat <- expand.grid(vars.values)
  ## create output matrix
  data.mat <- matrix(NA, ncol = 5, nrow = nrow(output.mat))
  colnames(data.mat) <- c("n.samples", "n.timeBins", "n.subregions", "avg.obs.per.timeBin", "avg.obs.per.subregion")
  ## create output list
  output.list <- lapply(1:nrow(output.mat), function(all) NA)
  ## create nested output object
  data.list <- lapply(1:nrow(time.cutoffs), function(all) mat <- data.mat)
  ## create nested model
  output.model.list <- lapply(1:nrow(time.cutoffs), function(all) mat <- output.list)
  ## populate output vectors
  for(i in 1:nrow(output.mat)){
    ## Read in data
    allData <- suppressWarnings(tryCatch(read.csv(input.dirs[i], row.names = 1, stringsAsFactors = T), error = function(e){}))
    ## if null, log entries as NA
    if(is.null(allData)){
      next
    } else {
      ## set reference category for full model
      allData[,"sampLith"] <- relevel(allData[,"sampLith"], ref = "mix")
      allData[,"sampEnv"] <- relevel(allData[,"sampEnv"], ref = "mix")
      allData[,"sampReef"] <- relevel(allData[,"sampReef"], ref = "mix")
      for(t in 1:nrow(time.cutoffs)){
        data <- allData[intersect(which(allData[,"times"] >= time.cutoffs[t,"top"]),which(allData[,"times"] <= time.cutoffs[t,"bottom"])),]
        data.list[[t]][i,1] <- nrow(data)
        data.list[[t]][i,2] <- length(unique(data[,"times"]))
        data.list[[t]][i,3] <- length(unique(data[,"source.subregion.ID"]))
        data.list[[t]][i,4] <- nrow(data)/(length(unique(data[,"times"])))
        data.list[[t]][i,5] <- nrow(data)/(length(unique(data[,"source.subregion.ID"])))
        mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + sampLith + sampEnv + sampReef + sampLat + (1|times/source.subregion.ID), data = data), error = function(e){}))
        if(is.null(mlm)){
          next
        } else {
          ## Add MLM to list
          output.model.list[[t]][[i]] <- mlm
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
