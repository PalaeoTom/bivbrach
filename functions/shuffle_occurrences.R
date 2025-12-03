shuffle_occurrences <- function(data, reps, stage, cell, cell_abun, occ_covariates, cell_covariates, covariate_values, CR = TRUE, CR_nOccs = 20, fix_stages = T, n_cores = 1){
  ## break cell_abun into vectors
  cells <- cell_abun[,1]
  abun <- as.numeric(cell_abun[,2])
  ## Get row numbers as vector
  occ_n <- seq(1,nrow(data),1)
  new_cell <- rep(NA,nrow(data))
  ## if fix_stages = T, group occ_n by stage
  if(fix_stages){
    stages <- data[,stage]
    uniq_stages <- sort(unique(stages))
    ## for each stage, in order, get row numbers in stage
    pooled_occ_n <- mclapply(1:length(uniq_stages), mc.cores = n_cores, function(x){
      out <- occ_n[which(stages == uniq_stages[x])]
    })
    ## get stages of cells
    stages_of_cells <- str_split_i(cells, "_", 1)
    ## for each stage, in order, get cells
    pooled_cells <- mclapply(1:length(uniq_stages), mc.cores = n_cores, function(x){
      out <- cells[which(stages_of_cells == uniq_stages[x])]
    })
    ## for each stage, in order, get abundance of each stage-cell combination
    pooled_abun <- mclapply(1:length(uniq_stages), mc.cores = n_cores, function(x){
      out <- abun[which(stages_of_cells == uniq_stages[x])]
    })
  }
  ## Clean up cell and delete cell covariates column
  template <- data
  if(any(colnames(data) %in% cell_covariates)){
    template <- template[,-which(colnames(template) %in% cell_covariates)]
  }
  template[,cell] <- NA
  ## loop
  if(fix_stages){
    ## Loop through
    output <- mclapply(1:reps, mc.cores = n_cores, function(all){
      if(all(!is.na(cell_covariates))){
      while(TRUE){
        ## copy list of cells
        out <- new_cell
        ## copy row IDs
        rows <- pooled_occ_n
        ## per stage
        for(s in 1:length(pooled_cells)){
          ## Shuffle them by sampling
          all_samp <- sample(rows[[s]], sum(pooled_abun[[s]]))
          ## Duplicate grid cell IDs as below
          labels <- rep(pooled_cells[[s]], pooled_abun[[s]])
          ## Assign shuffled row IDs to grid cells
          out[all_samp] <- labels
        }
        ## Re-attach to template
        perm <- template
        perm[,cell] <- out
        ## Now update covariates
        for(c in 1:length(cell_covariates)){
          perm[,cell_covariates[c]] <- get.cell.covariate(perm, cell, unknown = "unknown", ref = occ_covariates[c], value = covariate_values[c])
        }
        if(all(!is.na(perm[,cell_covariates]))){
          break
        }
      }
      } else {
        ## copy list of cells
        out <- new_cell
        ## copy row IDs
        rows <- pooled_occ_n
        ## per stage
        for(s in 1:length(pooled_cells)){
          ## Shuffle them by sampling
          all_samp <- sample(rows[[s]], sum(pooled_abun[[s]]))
          ## Duplicate grid cell IDs as below
          labels <- rep(pooled_cells[[s]], pooled_abun[[s]])
          ## Assign shuffled row IDs to grid cells
          out[all_samp] <- labels
        }
        ## Re-attach to template
        perm <- template
        perm[,cell] <- out
      }
      ## Now convert perm to grid cell output
      perm_cell_SCs <- unique(perm[,cell])
      perm_cell_S <- as.numeric(str_split_i(perm_cell_SCs, "_", 1))
      perm_cell_C <- as.numeric(str_split_i(perm_cell_SCs, "_", 2))
      perm_cell <- data.frame(cbind("stage_cell" = perm_cell_SCs, "stage" = as.numeric(perm_cell_S), "cell" = perm_cell_C))
      ## Get cell richness
      if(CR){
        rich_counts <- CR_richness(data = perm, n = CR_nOccs, cell = cell, reps = 100)
        perm_cell[,c("bivalve","brachiopod")] <- rich_counts[match(as.character(perm_cell[,cell]), as.character(rich_counts[,cell])),c(2,3)]
      } else {
        rich_counts <- raw_richness(data = perm)
        perm_cell[,c("bivalve","brachiopod")] <- rich_counts[match(as.character(perm_cell[,cell]), as.character(rich_counts[,cell])),c(2,3)]
      }
      ## Get cell covariates
      perm_cell[,"cellPalaeoLng"] <- extract_cell_metaData(perm, target = "cellx_100km", cell)
      perm_cell[,"cellPalaeoLat"] <- extract_cell_metaData(perm, target = "celly_100km", cell)
      perm_cell[,"cellAbsLat"] <- abs(perm_cell[,"cellPalaeoLat"])
      if(all(!is.na(cell_covariates))){
        for(c in 1:length(cell_covariates)){
          perm_cell[,cell_covariates[c]] <- extract_cell_metaData(perm, target = cell_covariates[c], cell)
        }
      }
      ## Create PTME
      PTME <- rep("PrePTME", nrow(perm_cell))
      PTME[which(as.numeric(perm_cell[,stage]) > 48)] <- "PostPTME"
      perm_cell[,"PTME"] <- PTME
      return(perm_cell)
    })
    return(output)
  } else {
    output <- mclapply(1:reps, mc.cores = n_cores, function(all){
      if(all(!is.na(cell_covariates))){
        while(TRUE){
          ## copy list of cells
            out <- new_cell
            ## copy row IDs
            rows <- occ_n
            ## Shuffle them by sampling
            all_samp <- sample(rows, sum(abun))
            ## duplicate grid cell IDs
            labels <- rep(cells, abun)
            ## Assign shuffled row IDs to grid cells
            out[all_samp] <- labels
            ## Re-attach to template
            perm <- template
            perm[,cell] <- out
            ## Now update covariates
            for(c in 1:length(cell_covariates)){
              perm[,cell_covariates[c]] <- get.cell.covariate(perm, cell, unknown = "unknown", ref = occ_covariates[c], value = covariate_values[c])
            }
            if(all(!is.na(perm[,cell_covariates]))){
              break
            }
          }
        } else {
          ## copy list of cells
          out <- new_cell
          ## copy row IDs
          rows <- occ_n
          ## Shuffle them by sampling
          all_samp <- sample(rows, sum(abun))
          ## duplicate grid cell IDs
          labels <- rep(cells, abun)
          ## Assign shuffled row IDs to grid cells
          out[all_samp] <- labels
          ## Re-attach to template
          perm <- template
          perm[,cell] <- out
        }
      ## Now convert perm to grid cell output
      perm_cell_SCs <- unique(perm[,cell])
      perm_cell_S <- as.numeric(str_split_i(perm_cell_SCs, "_", 1))
      perm_cell_C <- as.numeric(str_split_i(perm_cell_SCs, "_", 2))
      perm_cell <- data.frame(cbind("stage_cell" = perm_cell_SCs, "stage" = as.numeric(perm_cell_S), "cell" = perm_cell_C))
      ## Get cell richness
      if(CR){
        rich_counts <- CR_richness(data = perm, n = CR_nOccs, cell = cell, reps = 100)
        perm_cell[,c("bivalve","brachiopod")] <- rich_counts[match(as.character(perm_cell[,cell]), as.character(rich_counts[,cell])),c(2,3)]
      } else {
        rich_counts <- raw_richness(data = perm)
        perm_cell[,c("bivalve","brachiopod")] <- rich_counts[match(as.character(perm_cell[,cell]), as.character(rich_counts[,cell])),c(2,3)]
      }
      ## Get cell covariates
      perm_cell[,"cellPalaeoLng"] <- extract_cell_metaData(perm, target = "cellx_100km", cell)
      perm_cell[,"cellPalaeoLat"] <- extract_cell_metaData(perm, target = "celly_100km", cell)
      perm_cell[,"cellAbsLat"] <- abs(perm_cell[,"cellPalaeoLat"])
      if(all(!is.na(cell_covariates))){
        for(c in 1:length(cell_covariates)){
          perm_cell[,cell_covariates[c]] <- extract_cell_metaData(perm, target = cell_covariates[c], cell)
        }
      }
      ## Create PTME
      PTME <- rep("PrePTME", nrow(perm_cell))
      PTME[which(as.numeric(perm_cell[,stage]) > 48)] <- "PostPTME"
      perm_cell[,"PTME"] <- PTME
      return(perm_cell)
    })
    return(output)
  }
}
