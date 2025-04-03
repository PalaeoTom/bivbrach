get.units.wrapper <- function(forms, addenda = c("limestone", "limestones", "shale", "shales", "clay", "clays", "dolomite", "dolomites", "chalk", "chalks", "marble", "marbles", "schist", "schists", "chert", "cherts", "conglomerate", "conglomerates", "sandstone", "sandstones")){
  ms <- data.frame(forms,forms,forms)
  ## Assign categories
  rank <- rep("Fm", nrow(ms))
  rank[which(str_detect(ms[,1], " group$"))] <- "Gp"
  rank[which(str_detect(ms[,1], " supergroup$"))] <- "SGp"
  ## Strip series, subgroup, supergroup, group, and formation
  ms[,2] <- str_replace(ms[,2], " formation$", "")
  ms[,2] <- str_replace(ms[,2], " series$", "")
  ms[,2] <- str_replace(ms[,2], " subgroup$", "")
  ms[,2] <- str_replace(ms[,2], " supergroup$", "")
  ms[,2] <- str_replace(ms[,2], " group$", "")
  ## Combine
  ms <- cbind(ms, rank, rank)
  colnames(ms) <- c("verbatim_name", "name", "updated_name", "rank", "updated_rank")
  ## Add additional output columns
  ms$interval <- NA
  ms$t_age <- NA
  ms$b_age <- NA
  ms$lithology <- ""
  ms$environment <- ""
  ms$name_updated <- F
  ms$rank_updated <- F
  ms$both_updated <- F
  ms$check <- ""
  ## Pull data and populate
  for(i in 1:nrow(ms)){
    print(i)
    ## check there is some sort of result
    skip <- F
    ## catch if an error
    tryCatch({
      unit <- get_units(strat_name = ms[i,"name"])
    }, error = function(e){skip <<- T})
    if(skip) next
    ## catch if a warning message
    if(length(unit) == 0){
      skip <- T
    }
    if(skip) next
    ## check for exact match or exact match plus a space. If none, skip
    if(all(!str_detect(unlist(unit[,c(8:11)]), pattern = regex(paste0("^",ms[i,"name"],"$"), ignore_case = T))) && all(!str_detect(unlist(unit[,c(8:11)]), pattern = regex(paste0("^",ms[i,"name"]," "), ignore_case = T)))){
      rm(unit)
      next
    }
    ## At this point, we know we have an exact match or an exact match plus a space. Not both.
    ## If an exact match, we just need to find it and update it. Start with specified formation, try others if not try other ranks
    if(any(str_detect(unlist(unit[,c(8:11)]), pattern = regex(paste0("^",ms[i,"name"],"$"), ignore_case = T)))){
      ref <- unit[str_detect(unit[,ms[i,"updated_rank"]], pattern = regex(paste0("^",ms[i,"name"],"$"), ignore_case = T)),]
      if(nrow(ref)==0){
        ## look at other ranks and assign
        ms[i,"updated_rank"] <- colnames(unit[,c(8:11)])[which(apply(unit[,c(8:11)], 2, function(x) any(str_detect(x, pattern = regex(paste0("^",ms[i,"name"],"$"), ignore_case = T)))))]
        ## set ranks updated to T
        ms[i,"rank_updated"] <- T
        ## update name
        ms[i,"updated_name"] <- paste0(ms[i,"name"]," ", ms[i,"updated_rank"])
        ## get
        ref <- unit[str_detect(unit[,ms[i,"updated_rank"]], pattern = regex(paste0("^",ms[i,"name"],"$"), ignore_case = T)),]
      }
    } else {
      ## if not a perfect match, find match from list
      ## get names to attempt
      ad.names <- paste0(ms[i,"name"], " ", addenda)
      ## check if any are present
      checker1 <- c()
      for(c in ad.names){
        checker1 <- c(checker1, any(str_detect(unit[,ms[i,"updated_rank"]], pattern = regex(paste0("^",c,"$"), ignore_case = T))))
      }
      if(any(checker1)){
        ad.match <- ad.names[checker1]
        ## if more than one match, report issue and skip to next
        if(length(ad.match)>1){
          ms[i,"check"] <- paste0('More than one match: ', str_flatten(ad.match, ","))
          rm(unit)
          next
        } else {
          ## Otherwise, if one match, update name, mark tracker, and get ref
          ms[i,"name"] <- ad.match
          ms[i,"updated_name"] <- paste0(ad.match, " ", ms[i,"updated_rank"])
          ## update tracker
          ms[i,"name_updated"] <- T
          ## Get ref
          ref <- unit[str_detect(unit[,ms[i,"updated_rank"]], pattern = regex(paste0("^",ms[i,"name"],"$"), ignore_case = T)),]
        }
      } else {
        ## If none present in set rank, try other ranks
        checker2 <- c()
        for(c in ad.names){
          checker2 <- c(checker2, any(apply(unit[,c(8:11)], 2, function(x) any(str_detect(x, pattern = regex(paste0("^",c,"$"), ignore_case = T))))))
        }
        if(any(checker2)){
          ad.match <- ad.names[checker2]
          if(length(ad.match)>1){
            ms[i,"check"] <- paste0('More than one match: ', str_flatten(ad.match, ","))
            rm(unit)
            next
          } else {
            ## Just one match. Update rank and name
            ms[i,"updated_rank"] <- colnames(unit[,c(8:11)])[which(apply(unit[,c(8:11)], 2, function(x) any(str_detect(x, pattern = regex(paste0("^",ad.match,"$"), ignore_case = T)))))]
            ## Update name
            ms[i,"name"] <- ad.match
            ms[i,"updated_name"] <- paste0(ad.match, " ", ms[i,"updated_rank"])
            ## Update trackers
            ms[i,"name_updated"] <- T
            ms[i,"rank_updated"] <- T
            ms[i,"both_updated"] <- T
            ## Get ref
            ref <- unit[str_detect(unit[,ms[i,"updated_rank"]], pattern = regex(paste0("^",ms[i,"name"],"$"), ignore_case = T)),]
          }
        } else {
          ## if no match across any ranking, clearly an addendum is missing.
          ms[i,"check"] <- 'Unlisted addendum'
          rm(unit)
          next
        }
      }
    }
    ## Get intervals
    interval <- c(ref[which(ref[,"t_age"] == min(ref[,"t_age"])),"t_int_name"],ref[which(ref[,"b_age"] == max(ref[,"b_age"])),"b_int_name"])
    interval <- unique(interval)
    ## if length of interval above 1, flatten
    if(length(interval)>1){
      interval <- str_flatten(interval, collapse = ",")
    }
    ## Populate rows
    ms[i,"interval"] <- interval
    ms[i,"t_age"] <- min(ref[,"t_age"])
    ms[i,"b_age"] <- max(ref[,"b_age"])
    ## Get lith
    ms[i,"lithology"] <- str_flatten(unique(as.vector(unlist(sapply(1:length(ref[,"lith"]), function(x) ref$lith[[x]]$name)))), ",")
    ms[i,"environment"] <- str_flatten(unique(as.vector(unlist(sapply(1:length(ref[,"environ"]), function(x) ref$environ[[x]]$name)))), ",")
    rm(unit)
  }
  ## Pass over updated name, replacing abbreviations with full names
  ms[,"updated_name"] <- str_replace_all(ms[,"updated_name"], pattern = " Gp$", " group")
  ms[,"updated_name"] <- str_replace_all(ms[,"updated_name"], pattern = " SGp$", " supergroup")
  ms[,"updated_name"] <- str_replace_all(ms[,"updated_name"], pattern = " Fm$", " formation")
  ms[,"updated_name"] <- str_replace_all(ms[,"updated_name"], pattern = " Mbr$", " member")
  ## return ms
  return(ms)
}
