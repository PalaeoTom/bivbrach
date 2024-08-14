applyRefCollThresh <- function(data, threshold = 3, mode = "references", n.cores = 4){
  times <- names(data)
  if(mode == "references"){
    output <- mclapply(1:length(data), mc.cores = n.cores, function(x){
      if(nrow(data[[x]]) > 0){
        ## grid cell numbers
        grid.cells <- unique(data[[x]][,"cell"])
        ## initialize drop vector
        dropper <- c()
        ## get reference counts for bivalves and brachiopods for each
        for(i in 1:length(grid.cells)){
          gc <- data[[x]][which(data[[x]][,"cell"] %in% grid.cells[i]),]
          ref.comps <- distinct(gc[,c("reference_no","reference_comp")])[,2]
          brach.count <- length(which(ref.comps %in% "brach")) + length(which(ref.comps %in% "both"))
          biv.count <- length(which(ref.comps %in% "biv")) + length(which(ref.comps %in% "both"))
          if(brach.count < threshold || biv.count < threshold){
            dropper <- c(dropper, grid.cells[i])
          }
        }
        if(length(dropper) > 0){
          out <- data[[x]][-which(data[[x]][,"cell"] %in% dropper),]
        } else {
          out <- data[[x]]
        }
      } else {
        out <- data[[x]]
      }
      return(out)
    })
  }
  if(mode == "collections"){
    output <- mclapply(1:length(data), mc.cores = n.cores, function(x){
      if(nrow(data[[x]]) > 0){
        ## grid cell numbers
        grid.cells <- unique(data[[x]][,"cell"])
        ## initialize drop vector
        dropper <- c()
        ## get reference counts for bivalves and brachiopods for each
        for(i in 1:length(grid.cells)){
          gc <- data[[x]][which(data[[x]][,"cell"] %in% grid.cells[i]),]
          coll.comps <- distinct(gc[,c("collection_no","collection_comp")])[,2]
          brach.count <- length(which(coll.comps %in% "brach")) + length(which(coll.comps %in% "both"))
          biv.count <- length(which(coll.comps %in% "biv")) + length(which(coll.comps %in% "both"))
          if(brach.count < threshold || biv.count < threshold){
            dropper <- c(dropper, grid.cells[i])
          }
        }
        if(length(dropper) > 0){
          out <- data[[x]][-which(data[[x]][,"cell"] %in% dropper),]
        } else {
          out <- data[[x]]
        }
      } else {
        out <- data[[x]]
      }
      return(out)
    })
  }
  names(output) <- times
  return(output)
}
