update.intervals <- function(intervals, update, update_source, name = "Interval", FAD = "FAD", LAD = "LAD", FAD_basis = "FAD_basis", LAD_basis = "LAD_basis", retain_boundary_alignment = T){
  ## Partition those intervals outside range - these will be untouched by the below
  ## LADs greater than greatest FAD
  older_ind <- which(intervals[,LAD] > max(update[,FAD]))
  if(length(older_ind)>0){
    older_ints <- intervals[older_ind,]
    intervals <- intervals[-older_ind,]
  }
  ## FADs smaller than smallest LAD
  younger_ind <- which(intervals[,FAD] < min(update[,LAD]))
  if(length(younger_ind)>0){
    younger_ints <- intervals[younger_ind,]
    intervals <- intervals[-younger_ind,]
  }
  ## Find original values for stages with new FADLADs - these values will be used to match boundaries
  pre_update <- intervals[which(intervals[,name] %in% update[,name]),]
  ## Sort descending
  pre_update <- pre_update[order(pre_update$FAD, decreasing=T), ]
  ## First things first, update stages
  for(i in 1:nrow(update)){
    ## Update FADLADs
    intervals[which(intervals[,name] %in% update[i,name]),FAD] <- update[i,FAD]
    intervals[which(intervals[,name] %in% update[i,name]),LAD] <- update[i,LAD]
    ## Update basis
    intervals[which(intervals[,name] %in% update[i,name]),FAD_basis] <- update_source
    intervals[which(intervals[,name] %in% update[i,name]),LAD_basis] <- update_source
  }
  ## Now update matching boundaries
  if(retain_boundary_alignment){
    ## Add trackers
    intervals$FAD_original <- T
    intervals$LAD_original <- T
    ## Update those of stages
    intervals$FAD_original[which(intervals[,FAD_basis] == update_source)] <- F
    intervals$LAD_original[which(intervals[,LAD_basis] == update_source)] <- F
    ## FAD
    ## Update each FAD once
    for(i in 1:nrow(pre_update)){
      ## Get index
      ind <- which(intervals[,FAD] == pre_update[i,FAD])
      ## Trim down to those that haven't been changed yet
      ind <- ind[intervals[which(intervals[,FAD] == pre_update[i,FAD]),"FAD_original"]]
      ## If at least one boundary remains, update
      if(length(ind)>0){
        ## Update values
        intervals[ind,FAD] <- update[which(update[,name] == pre_update[i,name]),FAD]
        ## Update basis
        intervals[ind,FAD_basis] <- paste0(update_source,"_aligned_to_",pre_update[i,name])
        ## Update tracker
        intervals[ind,"FAD_original"] <- F
      }
    }
    ## Final FAD to update - those that match smallest LAD
    s_LAD_ind <- which(intervals[,FAD] == min(pre_update[,LAD]))
    ## If length of s_LAD_ind over 0
    if(length(s_LAD_ind>0)){
      ## Winnow down to that that haven't been updated
      s_LAD_ind <- s_LAD_ind[intervals[which(intervals[,FAD] == min(pre_update[,LAD])),"FAD_original"]]
      if(length(s_LAD_ind>0)){
        ## Update values
        intervals[s_LAD_ind,FAD] <- update[which(update[,name]==pre_update[which(pre_update[,LAD]==min(pre_update[,LAD])),name]),LAD]
        ## Update basis
        intervals[s_LAD_ind,FAD_basis] <- paste0(update_source,"_aligned_to_",pre_update[which(pre_update[,LAD]==min(pre_update[,LAD])),name])
        ## Update tracker
        intervals[s_LAD_ind,"FAD_original"] <- F
      }
    }
    ## LAD
    ## Update each LAD once
    for(i in 1:nrow(pre_update)){
      ## Get index
      ind <- which(intervals[,LAD] == pre_update[i,LAD])
      ## Trim down to those that haven't been changed yet
      ind <- ind[intervals[which(intervals[,LAD] == pre_update[i,LAD]),"LAD_original"]]
      ## If at least one boundary remains, update
      if(length(ind)>0){
        ## Update values
        intervals[ind,LAD] <- update[which(update[,name] == pre_update[i,name]),LAD]
        ## Update basis
        intervals[ind,LAD_basis] <- paste0(update_source,"_aligned_to_",pre_update[i,name])
        ## Update tracker
        intervals[ind,"LAD_original"] <- F
      }
    }
    ## Final LAD to update - those that match largest FAD
    l_FAD_ind <- which(intervals[,LAD] == max(pre_update[,FAD]))
    ## If length of l_FAD_ind over 0
    if(length(l_FAD_ind>0)){
      ## Winnow down to that that haven't been updated
      l_FAD_ind <- l_FAD_ind[intervals[which(intervals[,LAD] == max(pre_update[,FAD])),"FAD_original"]]
      if(length(l_FAD_ind>0)){
        ## Update values
        intervals[l_FAD_ind,LAD] <- update[which(update[,name]==pre_update[which(pre_update[,FAD]==max(pre_update[,FAD])),name]),FAD]
        ## Update basis
        intervals[l_FAD_ind,LAD_basis] <- paste0(update_source,"_aligned_to_",pre_update[which(pre_update[,FAD]==max(pre_update[,FAD])),name])
        ## Update tracker
        intervals[l_FAD_ind,"LAD_original"] <- F
      }
    }
    ## Remove trackers
    intervals$FAD_original <- NULL
    intervals$LAD_original <- NULL
  }
  ## Reattach unchanged intervals
  if(length(older_ind)>0 && length(younger_ind)>0){
    intervals <- rbind(older_ints,younger_ints,intervals)
  } else {
    if(length(older_ind)>0){
      intervals <- rbind(older_ints,intervals)
    }
    if(length(younger_ind)>0){
      intervals <- rbind(younger_ints,intervals)
    }
  }
  ## Return intervals
  return(intervals)
}
