shuffle_responses <- function(data, reps, stage, response, predictor, standardise, shuffle_predictor = F, fix_stages = T, n_cores = 1){
  ## Empty out templates
  template <- data
  ## Clean out response
  template[,response] <- NA
  ## Clean out predictor if going to be shuffled
  if(shuffle_predictor){
    template[,predictor] <- NA
  }
  ## loop
  if(fix_stages){
    ## Isolate stages and unique stages
    stages <- data[,stage]
    uniq_stages <- sort(unique(stages))
    ## for each stage, in order, get row numbers in stage
    pooled_row_n <- mclapply(1:length(uniq_stages), mc.cores = n_cores, function(x){
      out <- which(stages == uniq_stages[x])
    })
    ## Shuffle predictor and response
    if(shuffle_predictor){
      output <- mclapply(1:reps, mc.cores = n_cores, function(all){
        ## Create copy
        perm <- template
        ## for each stage
        for(s in 1:length(pooled_row_n)){
          perm[pooled_row_n[[s]],c(predictor, response)] <- sample(unlist(data[pooled_row_n[[s]], c(predictor,response)]))
        }
        ## Round response
        perm[,response] <- round(perm[,response], digits = 0)
        ## Standardise predictors
        perm[,standardise] <- std(perm, perm[,standardise])[,seq(ncol(perm)+1, ncol(perm)+length(standardise),1)]
        ## return perm
        return(perm)
      })
    } else {
      output <- mclapply(1:reps, mc.cores = n_cores, function(all){
        ## Create copy
        perm <- template
        ## for each stage
        for(s in 1:length(pooled_row_n)){
          perm[pooled_row_n[[s]],response] <- sample(data[pooled_row_n[[s]],response])
        }
        ## Round response
        perm[,response] <- round(perm[,response], digits = 0)
        ## Standardise predictors
        perm[,standardise] <- std(perm, perm[,standardise])[,seq(ncol(perm)+1, ncol(perm)+length(standardise),1)]
        ## return perm
        return(perm)
      })
    }
  } else {
    if(shuffle_predictor){
      output <- mclapply(1:reps, mc.cores = n_cores, function(all){
        ## Create copy
        perm <- template
        ## for each stage
        perm[,c(predictor, response)] <- sample(unlist(data[,c(predictor,response)]))
        ## Round response
        perm[,response] <- round(perm[,response], digits = 0)
        ## Standardise predictors
        perm[,standardise] <- std(perm, perm[,standardise])[,seq(ncol(perm)+1, ncol(perm)+length(standardise),1)]
        ## return perm
        return(perm)
      })
    } else {
      output <- mclapply(1:reps, mc.cores = n_cores, function(all){
        ## Create copy
        perm <- template
        ## for each stage
        perm[,response] <- sample(data[,response])
        ## Round response
        perm[,response] <- round(perm[,response], digits = 0)
        ## Standardise predictors
        perm[,standardise] <- std(perm, perm[,standardise])[,seq(ncol(perm)+1, ncol(perm)+length(standardise),1)]
        ## return perm
        return(perm)
      })
    }
  }
  return(output)
}
