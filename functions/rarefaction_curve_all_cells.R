rarefaction_curve_all_cells2 <- function(data, cell, taxVar, iter, n.cores = 1){
  occs <- data[[taxVar]]
  cells <- data[[cell]]
  occs_by_cell <- split(occs, cells)
  cells_u <- names(occs_by_cell)
  n_cells <- length(cells_u)
  allCells_mean <- mclapply(occs_by_cell, mc.cores = n.cores, function(samp){
    rarefaction_curve(samp, iter)
  })
  max_n <- max(vapply(allCells_mean, length, numeric(1)))
  output_mat <- matrix(NA, nrow = max_n, ncol = n_cells)
  colnames(output_mat) <- cells_u
  for(i in seq_along(allCells_mean)){
    len_i <- length(allCells_mean[[i]])
    output_mat[1:len_i, i] <- allCells_mean[[i]]
  }
  output_df <- data.frame(sampled = seq_len(max_n), output_mat, check.names = FALSE)
  return(output_df)
}
