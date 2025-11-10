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
    uniq_stages <- unique(stages)
    pooled_occ_n <- mclapply(1:length(uniq_stages), mc.cores = n_cores, function(x){
      out <- occ_n[which(stages == uniq_stages[x])]
    })
    stages_of_cells <- str_split_i(cells, "_", 1)
    pooled_cells <- mclapply(1:length(uniq_stages), mc.cores = n_cores, function(x){
      out <- cells[which(stages_of_cells == uniq_stages[x])]
    })
    pooled_abun <- mclapply(1:length(uniq_stages), mc.cores = n_cores, function(x){
      out <- abun[which(stages_of_cells == uniq_stages[x])]
    })
  }
  ## Clean up cell and delete cell covariates column
  template <- data[,-which(colnames(data) %in% cell_covariates)]
  template[,cell] <- NA
  ## loop
  if(fix_stages){
    ## Loop through
    output <- mclapply(1:reps, mc.cores = n_cores, function(all){
      while(TRUE){
        ## copy list of cells
        out <- new_cell
        ## copy row IDs
        rows <- pooled_occ_n
        ## Loop through
        for(s in 1:length(pooled_cells)){
          for(i in 1:length(pooled_cells[[s]])){
            ## sample abun[i] row numbers
            samp <- sample(rows[[s]],pooled_abun[[s]][i])
            ## update out with new stage ID
            out[samp] <- pooled_cells[[s]][i]
            ## drop selected cells from rows
            rows[[s]] <- rows[[s]][!rows[[s]] %in% samp]
          }
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
      ## Now convert perm to grid cell output
      perm_cell_SCs <- unique(perm[,cell])
      perm_cell_S <- as.numeric(str_split_i(perm_cell_SCs, "_", 1))
      perm_cell_C <- as.numeric(str_split_i(perm_cell_SCs, "_", 2))
      perm_cell <- data.frame(cbind("stage_cell" = perm_cell_SCs, "stage" = as.numeric(perm_cell_S), "cell" = perm_cell_C))
      ## Get cell richness
      if(CR){
        perm_cell[,c("bivalve","brachiopod")] <- CR_richness(data = perm, n = CR_nOccs, n.cores = 1)[,c(2,3)]
      } else {
        perm_cell[,c("bivalve","brachiopod")] <- raw_richness(data = perm, n.cores = 1)[,c(2,3)]
      }
      ## Get cell covariates
      perm_cell[,"cellPalaeoLng"] <- extract_cell_metaData(perm, target = "cellx_100km", cell)
      perm_cell[,"cellPalaeoLat"] <- extract_cell_metaData(perm, target = "celly_100km", cell)
      perm_cell[,"cellLith"] <- extract_cell_metaData(perm, target = "cellLith", cell)
      perm_cell[,"cellBath"] <- extract_cell_metaData(perm, target = "cellBath", cell)
      perm_cell[,"cellReef"] <- extract_cell_metaData(perm, target = "cellReef", cell)
      perm_cell[,"cellAbsLat"] <- abs(perm_cell[,"cellPalaeoLat"])
      ## Create PTME
      PTME <- rep("PrePTME", nrow(perm_cell))
      PTME[which(perm_cell[,stage] > 48)] <- "PostPTME"
      perm_cell[,"PTME"] <- PTME
      return(perm_cell)
    })
    return(output)
  } else {
    output <- mclapply(1:reps, mc.cores = n_cores, function(all){
      while(TRUE){
        ## copy list of cells
        out <- new_cell
        ## copy row IDs
        rows <- occ_n
        for(i in 1:length(cells)){
          ## sample abun[i] row numbers
          samp <- sample(rows,abun[i])
          ## update out with new stage ID
          out[samp] <- cells[i]
          ## drop selected cells from rows
          rows <- rows[!rows %in% samp]
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
      ## Now convert perm to grid cell output
      perm_cell_SCs <- unique(perm[,cell])
      perm_cell_S <- as.numeric(str_split_i(perm_cell_SCs, "_", 1))
      perm_cell_C <- as.numeric(str_split_i(perm_cell_SCs, "_", 2))
      perm_cell <- data.frame(cbind("stage_cell" = perm_cell_SCs, "stage" = as.numeric(perm_cell_S), "cell" = perm_cell_C))
      ## Get cell richness
      if(CR){
        perm_cell[,c("bivalve","brachiopod")] <- CR_richness(data = perm, n = CR_nOccs, n.cores = 1)[,c(2,3)]
      } else {
        perm_cell[,c("bivalve","brachiopod")] <- raw_richness(data = perm, n.cores = 1)[,c(2,3)]
      }
      ## Get cell covariates
      perm_cell[,"cellPalaeoLng"] <- extract_cell_metaData(perm, target = "cellx_100km", cell)
      perm_cell[,"cellPalaeoLat"] <- extract_cell_metaData(perm, target = "celly_100km", cell)
      perm_cell[,"cellLith"] <- extract_cell_metaData(perm, target = "cellLith", cell)
      perm_cell[,"cellBath"] <- extract_cell_metaData(perm, target = "cellBath", cell)
      perm_cell[,"cellReef"] <- extract_cell_metaData(perm, target = "cellReef", cell)
      perm_cell[,"cellAbsLat"] <- abs(perm_cell[,"cellPalaeoLat"])
      ## Create PTME
      PTME <- rep("PrePTME", nrow(perm_cell))
      PTME[which(perm_cell[,stage] > 48)] <- "PostPTME"
      perm_cell[,"PTME"] <- PTME
      return(perm_cell)
    })
    return(output)
  }
}
