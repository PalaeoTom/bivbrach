mass.lmm <- function(input.dir, input.pre, output.dir, output.pre, vars, vars.values, type = "full"){
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
    } else {
      ## set reference category for full model
      data[,"sampLith"] <- relevel(data[,"sampLith"], ref = "mix")
      data[,"sampEnv"] <- relevel(data[,"sampEnv"], ref = "mix")
      data[,"sampReef"] <- relevel(data[,"sampReef"], ref = "mix")
      ## record metadata
      data.mat[i,1] <- nrow(data)
      data.mat[i,2] <- length(unique(data[,"times"]))
      data.mat[i,3] <- length(unique(data[,"source.subregion.ID"]))
      data.mat[i,4] <- nrow(data)/(length(unique(data[,"times"])))
      data.mat[i,5] <- nrow(data)/(length(unique(data[,"source.subregion.ID"])))
      if(type == "full"){
        ## check contrasts
        if(length(levels(data[,"sampLith"]))>1 && length(levels(data[,"sampEnv"]))>1 && length(levels(data[,"sampReef"]))>1){
        mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + sampLith + sampEnv + sampReef + sampLat + (1|times/source.subregion.ID), data = data), error = function(e){}))
        }
        if(!length(levels(data[,"sampLith"]))>1 && length(levels(data[,"sampEnv"]))>1 && length(levels(data[,"sampReef"]))>1){
          mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia  + sampEnv + sampReef + sampLat + (1|times/source.subregion.ID), data = data), error = function(e){}))
        }
        if(length(levels(data[,"sampLith"]))>1 && !length(levels(data[,"sampEnv"]))>1 && length(levels(data[,"sampReef"]))>1){
          mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + sampLith + sampReef + sampLat + (1|times/source.subregion.ID), data = data), error = function(e){}))
        }
        if(length(levels(data[,"sampLith"]))>1 && length(levels(data[,"sampEnv"]))>1 && !length(levels(data[,"sampReef"]))>1){
          mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + sampLith + sampEnv + sampLat + (1|times/source.subregion.ID), data = data), error = function(e){}))
        }
        if(length(levels(data[,"sampLith"]))>1 && !length(levels(data[,"sampEnv"]))>1 && !length(levels(data[,"sampReef"]))>1){
          mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + sampLith + sampLat + (1|times/source.subregion.ID), data = data), error = function(e){}))
        }
        if(!length(levels(data[,"sampLith"]))>1 && length(levels(data[,"sampEnv"]))>1 && !length(levels(data[,"sampReef"]))>1){
          mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + sampEnv + sampLat + (1|times/source.subregion.ID), data = data), error = function(e){}))
        }
        if(!length(levels(data[,"sampLith"]))>1 && !length(levels(data[,"sampEnv"]))>1 && length(levels(data[,"sampReef"]))>1){
          mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + sampReef + sampLat + (1|times/source.subregion.ID), data = data), error = function(e){}))
        }
        } else {
        if(type == "simple"){
          mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia + (1|times/source.subregion.ID), data = data), error = function(e){}))
        } else {
          if(type == "bivalveONLY"){
            mlm <- suppressMessages(tryCatch(lmer(Brachiopoda ~ Bivalvia, data = data), error = function(e){}))
          } else {
            stop("check 'type' is 'simple', 'full', or 'bivalveONLY'")
          }
        }
      }
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
