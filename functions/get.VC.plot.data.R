get.VC.plot.data <- function(UTB, split.vars){
  var.indices <- c()
  for (i in 1:ncol(split.vars)) var.indices <- c(var.indices, which(colnames(UTB) %in% colnames(split.vars)[i]))
  allVars <- lapply(1:ncol(split.vars), function(v){
    var1 <- lapply(1:nrow(split.vars), function(r){
      out <- which(UTB[,var.indices[v]] == split.vars[r,v])
    })
  })
  output <- lapply(1:length(allVars[[1]]), function(x){
    out <- UTB[intersect(allVars[[1]][[x]], allVars[[2]][[x]]),]
  })
}
