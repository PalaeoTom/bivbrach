bin.data <- function(occs, trunc.stages = NULL, complete.stages = NULL, max_time, min_time, bin_size, uniqify.data = T, uniqify.taxVar = "genus", uniqify.xy = c("cellX", "cellY")){
  if(is.null(trunc.stages)){
    ## get bins
    bins <- get.bins(max.t = max_time, min.t = min_time, bin.s = bin_size)
    ## get data
    if(uniqify.data){
      binned <- lapply(1:length(bins), function(x){
        out <- uniqify(extract.time.bin(data = occs, MA.start = bins[[x]][1], MA.end = bins[[x]][2]), taxVar = uniqify.taxVar, xy = uniqify.xy)
      })
    } else {
      binned <- lapply(1:length(bins), function(x){
        out <- extract.time.bin(data = occs, MA.start = bins[[x]][1], MA.end = bins[[x]][2])
      })
    }
  } else {
    binned <- lapply(1:(nrow(trunc.stages)-1), function(y){
      # Get stages to be included
      stage <- trunc.stages$name[y]
      next_stage <- trunc.stages$name[y+1]
      # include names of all bins that are lumped with the focal time bin
      included.stages <- complete.stages$name[which(complete.stages$name==stage):(which(complete.stages$name==next_stage)-1)]
      # get data
      if(uniqify.data){
        out <- uniqify(extract.stage.bin(data = occs, MA.start = trunc.stages$b_round[y], MA.end = trunc.stages$t_round[y], included = included.stages), taxVar = uniqify.taxVar, xy = uniqify.xy)
      } else {
        out <- extract.stage.bin(data = occs, MA.start = trunc.stages$b_round[y], MA.end = trunc.stages$t_round[y], included = included.stages)
      }
      return(out)
    })
  }
  return(binned)
}
