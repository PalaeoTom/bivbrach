interpolate.intervals <- function(intervals, original, interpolate, interpolate_source, ignore = NULL, name = "Interval", FAD = "FAD", LAD = "LAD", FAD_basis = "FAD_basis", LAD_basis = "LAD_basis", scheme = "type"){
  ## Partition those intervals outside range - these will be untouched by the below
  ## LADs greater than greatest FAD
  older_ind <- which(intervals[,LAD] > max(interpolate[,FAD]))
  if(length(older_ind)>0){
    older_ints <- intervals[older_ind,]
    intervals <- intervals[-older_ind,]
  }
  ## FADs smaller than smallest LAD
  younger_ind <- which(intervals[,FAD] < min(interpolate[,LAD]))
  if(length(younger_ind)>0){
    younger_ints <- intervals[younger_ind,]
    intervals <- intervals[-younger_ind,]
  }
  ## Same for original
  old_orig_ind <- which(original[,LAD] > max(interpolate[,FAD]))
  if(length(old_orig_ind)>0){
    older_orig_ints <- original[old_orig_ind,]
    original <- original[-old_orig_ind,]
  }
  ## FADs smaller than smallest LAD
  young_orig_ind <- which(original[,FAD] < min(interpolate[,LAD]))
  if(length(young_orig_ind)>0){
    younger_orig_ints <- original[young_orig_ind,]
    original <- original[-young_orig_ind,]
  }
  ## Check for no duplication in interpolate FADs and LAD
  if(any(duplicated(interpolate[,FAD])) || any(duplicated(interpolate[,LAD]))){
    stop("Duplicated FADs and/or LADs in interpolate")
  }
  ## Add trackers
  intervals$FAD_original <- T
  intervals$LAD_original <- T
  ## if ignore is not null, ensure all intervals with FAD_basis and/or LAD_basis including strings "ignore" are not changed.
  if(!is.null(ignore)){
    intervals$FAD_original[which(str_detect(intervals[,FAD_basis],ignore))] <- F
    intervals$LAD_original[which(str_detect(intervals[,LAD_basis],ignore))] <- F
  }
  # Identify schemes to work through
  schemes <- unique(intervals[,scheme])
  ## Now go row by row, working through interpolate
  i = 78
  for(i in 1:nrow(interpolate)){
    ## Get start and end of interval
    start <- interpolate[i,FAD]
    end <- interpolate[i,LAD]
    range <- start-end
    ## Get old start and end from original
    old_start <- original[which(original[,name]==interpolate[i,name]),"FAD"]
    old_end <- original[which(original[,name]==interpolate[i,name]),"LAD"]
    old_range <- old_start-old_end
    ## For each scheme
    s = schemes[64]
    for(s in schemes){
      ## Get index of scheme
      ind <- which(intervals[,scheme]==s)
      ## Refine to those bracketed by interval by interpolate
      as <- ind[which(intervals[ind,FAD]<=max(start,old_start))]
      be <- ind[which(intervals[ind,LAD]>=min(end,old_end))]
      ir <- intersect(as,be)
      ## If length is over 1, then we have boundaries within interval that may need updating
      if(length(ir)>0){
        ## Get old FADs
        old_FADs <- original[ir,FAD]
        ## Get old LADs
        old_LADs <- original[ir,LAD]
        LAD_ir <- FAD_ir <- ir
        ## Use one or both to reorder IR, then reorder each of these themselves
        FAD_ir <- FAD_ir[order(old_FADs,decreasing = T)]
        old_FADs <- old_FADs[order(old_FADs, decreasing = T)]
        LAD_ir <- LAD_ir[order(old_LADs,decreasing = T)]
        old_LADs <- old_LADs[order(old_LADs, decreasing = T)]
        ## Check if old_FADs includes old_start
        if(!old_start %in% old_FADs){
          old_FADs <- c(old_start,old_FADs)
          FAD_ir <- c(NA,FAD_ir)
        }
        ## Check if old_FADs includes old_end
        if(!old_end %in% old_FADs){
          old_FADs <- c(old_FADs, old_end)
          FAD_ir <- c(FAD_ir,NA)
        }
        ## Check if old_LADs includes old_start
        if(!old_start %in% old_LADs){
          old_LADs <- c(old_start,old_LADs)
          LAD_ir <- c(NA,LAD_ir)
        }
        ## Check if old_LADs includes old_end
        if(!old_end %in% old_LADs){
          old_LADs <- c(old_LADs, old_end)
          LAD_ir <- c(LAD_ir,NA)
        }
        ## Get proportion of range between old FADs and LADs
        old_FAD_prop <- diff(old_FADs)/old_range
        old_LAD_prop <- diff(old_LADs)/old_range
        ## Get new FADs and LADs by modifying the below
        new_FADs <- c(start,start+cumsum(old_FAD_prop)*range)
        new_LADs <- c(start,start+cumsum(old_LAD_prop)*range)
        ## Drop those with an IR of NA
        new_FADs <- new_FADs[!is.na(FAD_ir)]
        new_LADs <- new_LADs[!is.na(LAD_ir)]
        ## Drop NA from these irs
        FAD_ir <- FAD_ir[!is.na(FAD_ir)]
        LAD_ir <- LAD_ir[!is.na(LAD_ir)]
        ## Get Boolean for both FAD and LAD
        FAD_bool <- intervals[FAD_ir,"FAD_original"]
        LAD_bool <- intervals[LAD_ir,"LAD_original"]
        ## If any are original, interpolate
        if(any(FAD_bool)){
          ## interpolate values
          intervals[FAD_ir[FAD_bool],FAD] <- new_FADs[FAD_bool]
          ## interpolate basis
          intervals[FAD_ir[FAD_bool],FAD_basis] <- paste0(interpolate_source,"_interpolated")
          ## interpolate tracker
          intervals[FAD_ir[FAD_bool],"FAD_original"] <- F
        }
        if(any(LAD_bool)){
          ## interpolate values
          intervals[LAD_ir[LAD_bool],LAD] <- new_LADs[LAD_bool]
          ## interpolate basis
          intervals[LAD_ir[LAD_bool],LAD_basis] <- paste0(interpolate_source,"_interpolated")
          ## interpolate tracker
          intervals[LAD_ir[LAD_bool],"LAD_original"] <- F
        }
      }
    }
  }
  ## Remove trackers
  intervals$FAD_original <- NULL
  intervals$LAD_original <- NULL
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
