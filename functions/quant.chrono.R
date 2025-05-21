quant.chrono <- function(data, key, column = "chronostratigraphy"){
  ## add relevant columns
  data$early_interval <- NA
  data$late_interval <- NA
  data$max_ma <- NA
  data$min_ma <- NA
  data$midpoint <- NA
  ## Populate new cells using chronostratigraphy
  for(i in 1:nrow(data)){
    print(i)
    c <- data[i,column]
    ## if comma, split
    if(str_detect(c, fixed(","))){
      ## split
      sc <- unlist(str_split(c, fixed(",")))
      ## isolate intervals
      min_ma <- c()
      max_ma <- c()
      midpoint <- c()
      for(int in sc){
        min_ma <- c(min_ma,key[which(key[,"name"] %in% int),"t_age"])
        max_ma <- c(max_ma,key[which(key[,"name"] %in% int),"b_age"])
        midpoint <- c(midpoint,key[which(key[,"name"] %in% int),"Midpoint"])
      }
      ## Start with late interval
      late_int <- sc[which(min_ma %in% min(min_ma))]
      late_min_ma <- min_ma[which(min_ma %in% min(min_ma))]
      ## if length is two, take shortest interval
      if(length(late_int) > 1){
        late_max_ma <- max_ma[which(min_ma %in% min(min_ma))]
        ## get duration
        dur <- late_max_ma-late_min_ma
        ## retain minimum dur
        late_int <- late_int[which(dur %in% min(dur))]
        late_min_ma <- late_min_ma[which(dur %in% min(dur))]
      }
      if(length(late_int) > 1){
        stop(paste0("Row ", i, " has two intervals with matching durations that could fit in late_interval slot: ", late_int))
      } else {
        data[i,"late_interval"] <- late_int
        data[i,"min_ma"] <- late_min_ma
      }
      ## Now do the same for the early interval
      early_int <- sc[which(max_ma %in% max(max_ma))]
      early_max_ma <- max_ma[which(max_ma %in% max(max_ma))]
      ## if length is two, take shortest interval
      if(length(early_int) > 1){
        early_min_ma <- min_ma[which(max_ma %in% max(max_ma))]
        ## get duration
        dur <- early_max_ma-early_min_ma
        ## retain minimum dur
        early_int <- early_int[which(dur %in% min(dur))]
        early_max_ma <- early_max_ma[which(dur %in% min(dur))]
      }
      if(length(early_int) > 1){
        stop(paste0("Row ", i, " has two intervals with matching durations that could fit in early_interval slot: ", early_int))
      } else {
        data[i,"early_interval"] <- early_int
        data[i,"max_ma"] <- early_max_ma
      }
      ## Then assign midpoint
      data[i,"midpoint"] <- (early_max_ma+late_min_ma)/2
    } else {
      ## Add populate early interval
      data[i,"early_interval"] <- c
      ## Find interval in ms.strat
      data[i,"min_ma"] <- key[which(key[,"name"] %in% c),"t_age"]
      data[i,"max_ma"] <- key[which(key[,"name"] %in% c),"b_age"]
      data[i,"midpoint"] <- key[which(key[,"name"] %in% c),"Midpoint"]
    }
  }
  ## delete old column
  data[,column] <- NULL
  return(data)
}
