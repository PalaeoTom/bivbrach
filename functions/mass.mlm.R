mass.mlm <- function(input.dir, input.pre, output.dir, output.pre, vars, vars.values){
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".csv")
  output.dir.mlm <- paste0(output.dir, "/", output.pre, "_models.Rds")
  output.dir <- paste0(output.dir, "/", output.pre, ".csv")
  output.mat <- expand.grid(vars.values)
  ## create output matrix
  data.mat <- matrix(NA, ncol = 15, nrow = nrow(output.mat))
  colnames(data.mat) <- c("n.samples", "n.timeBins", "n.subregions", "avg.obs.per.timeBin", "avg.obs.per.subregion", "intercept", "intercept.std.error", "intercept.df", "intercept.t.value", "intercept.p.value", "slope", "slope.std.error", "slope.df", "slope.t.value", "slope.p.value")
  ## create output list
  output.list <- lapply(1:nrow(output.mat), function(all) NA)
  ## populate output vectors
  for(i in 1:nrow(output.mat)){
    ## Read in data
    data <- suppressWarnings(tryCatch(read.csv(input.dirs[i], row.names = 1), error = function(e){}))
    ## if null, log entries as NA
    if(is.null(data)){
      next
    } else {
      data.mat[i,1] <- nrow(data)
      data.mat[i,2] <- length(unique(data[,"times"]))
      data.mat[i,3] <- length(unique(data[,"source.subregion.ID"]))
      data.mat[i,4] <- nrow(data)/(length(unique(data[,"times"])))
      data.mat[i,5] <- nrow(data)/(length(unique(data[,"source.subregion.ID"])))
      mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + sampLith + sampEnv + sampReef + sampLat + (1|times/source.subregion.ID), data = data), error = function(e){}))
      if(is.null(mlm)){
        next
      } else {
        ## Add MLM to list
        output.list[[i]] <- mlm
      ## get coefficients
      coefficients <- coef(summary(mlm))
      data.mat[i,6] <- coefficients[1,1]
      data.mat[i,7] <- coefficients[1,2]
      data.mat[i,8] <- coefficients[1,3]
      data.mat[i,9] <- coefficients[1,4]
      data.mat[i,10] <- coefficients[1,5]
      data.mat[i,11] <- coefficients[2,1]
      data.mat[i,12] <- coefficients[2,2]
      data.mat[i,13] <- coefficients[2,3]
      data.mat[i,14] <- coefficients[2,4]
      data.mat[i,15] <- coefficients[2,5]
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
