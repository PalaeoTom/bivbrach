raw_richness <- function(data, cell = "stage_cell", taxon_name = "combined_name", count = c("Mollusca", "Brachiopoda"), n.cores = 1){
  ## Get cells
  cells <- unique(as.character(data[,cell]))
  ## Define number of patterns
  K <- length(count)
  patterns <- count
  ## Subset taxa into list of cells
  cell_keys <- as.character(data[,cell])
  taxon_list <- split(as.character(data[,taxon_name]), cell_keys)
  TL_sorted <- taxon_list[match(cells, as.character(names(taxon_list)))]
  ## define worker function to get raw richness of each cell
  worker <- function(ix){
    taxa_vec <- TL_sorted[[ix]]
    # Get unique taxa
    uniq_taxa <- unique(taxa_vec)
    # Get number of unique taxa
    n_unique <- length(uniq_taxa)
    # Define for matching
    matches <- matrix(FALSE, nrow = n_unique, ncol = K)
    ## Assign logical (TRUE treated as 1, FALSE as 0)
    for(j in seq_len(K)){
      matches[, j] <- grepl(patterns[j], uniq_taxa, perl = TRUE)
    }
    colSums(matches)
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
