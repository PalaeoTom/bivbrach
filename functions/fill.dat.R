fill.dat <- function(bin, occ_dat, env_dat, bin_col, cell_col, env_axes){
  occ_bin <- occ_dat[ occ_dat[,bin_col]==bin, ]
  env_bin <- data.frame(sapply(env_dat, function(r) getValues(r[[bin]]) ))
  colnames(env_bin) <- env_axes
  new_dat <- sapply(1:nrow(occ_bin), function(i){
    record <- occ_bin[i,]
    if (any(is.na(record[env_axes])==TRUE)){
      env2infer <- env_axes[which(is.na(record[env_axes])==TRUE)]
      cell <- as.numeric(record[cell_col])
      record[env2infer] <- env_bin[cell,env2infer]
      output <- record[env_axes]
    } else { output <- record[env_axes] }
    output <- as.matrix(output)
  })
  t(new_dat)
}
