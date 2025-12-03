CR_richness <- function(data, n = 20, reps = 20, cell = "stage_cell", taxon_name = "combined_name", count = c("Mollusca", "Brachiopoda"), summarise = "median", n.cores = 1){
  ## Get cells
  cells <- unique(as.character(data[,cell]))
  # handle n argument (vector or single value)
  if (length(n) == 1) {
    n <- rep(n, length(cells))
  } else {
    if(length(n) != length(cells)) stop("n does not include values for every cell in data")
    # ensure ordering of n matches order of cells if n had names
    if(is.null(names(n))) stop("n does not have names")
    n <- n[match(cells, as.character(names(n)))]
  }
  ## Define number of patterns
  K <- length(count)
  patterns <- count
  ## Subset taxa into list
  cell_keys <- as.character(data[,cell])
  taxon_list <- split(as.character(data[,taxon_name]), cell_keys)
  TL_sorted <- taxon_list[match(cells, as.character(names(taxon_list)))]
  ## Define worker function for a single cell
  worker <- function(ix){
    taxa_vec <- TL_sorted[[ix]]
    L <- length(taxa_vec)
    # Map taxa names to integer ids (unique taxa)
    uniq_taxa <- unique(taxa_vec)
    id_of_name <- match(taxa_vec, uniq_taxa)
    # Precompute matches per unique taxon (logical matrix: n_unique_taxa x K)
    n_unique <- length(uniq_taxa)
    matches <- matrix(FALSE, nrow = n_unique, ncol = K)
    for(j in seq_len(K)){
      matches[, j] <- grepl(patterns[j], uniq_taxa, perl = TRUE)
    }
    # Preallocate draws matrix: reps x K
    draws <- matrix(0L, nrow = reps, ncol = K)
    # Run sampling reps times
    for (r in seq_len(reps)) {
      # sample specimen indices (faster to sample ints)
      samp_idx <- sample.int(L, size = n[ix], replace = FALSE)
      samp_ids <- unique(id_of_name[samp_idx])
      # count matched unique taxa per pattern
      # then sum over rows of matches for the sampled unique ids
      if(length(samp_ids) == 1L){
        draws[r, ] <- as.integer(matches[samp_ids, ])
      } else {
        draws[r, ] <- as.integer(colSums(matches[samp_ids, , drop = FALSE]))
      }
    }
    # summarise draws across reps
    if (summarise == "median") {
      apply(draws, 2, stats::median)
    } else {
      apply(draws, 2, mean)
    }
  }
  # Parallel loop across cells
  out_list <- mclapply(seq_along(cells), worker, mc.cores = n.cores)
  # combine results
  final_mat <- do.call(rbind, out_list)
  colnames(final_mat) <- count
  final_df <- cbind(cell = cells, as.data.frame(final_mat, stringsAsFactors = FALSE))
  names(final_df)[1] <- cell
  return(final_df)
}

