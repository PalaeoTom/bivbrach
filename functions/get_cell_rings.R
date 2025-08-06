get_cell_rings <- function(raster, cell, max_ring = 5, n.cores = 1){
  ## raster details
  nrow <- nrow(raster)
  ncol <- ncol(raster)
  ## find cell coords in raster
  cell_row <- rowFromCell(raster, cell)
  cell_col <- colFromCell(raster, cell)
  ## find coordinates of rings
  output <- mclapply(1:max_ring, mc.cores = n.cores, function(x){
    ## Get left column
    left.col <- cell_col-x
    ## If left is less than 1, subtract it from rightmost cell of that row
    if(left.col < 1){
      left.col <- left.col+ncol
    }
    ## Get right
    right.col <- cell_col+x
    ## If right is more than ncol, subtract ncol
    if(right.col > ncol){
      right.col <- right.col-ncol
    }
    ## Get top
    top.row <- cell_row-x
    ## If row is less than 1, add to number of row
    if(top.row < 1){
      top.row <- top.row + nrow
    }
    ## Get bottom
    bottom.row <- cell_row+x
    ## If bottom row is greater than nrow, subtract nrow
    if(bottom.row > nrow){
      bottom.row <- bottom.row-nrow
    }
    ## Have columns and rows. Now to get all cells involved
    ## Get left and right
    lr_rows <- seq(cell_row-x,cell_row+x,1)
    ## Correct those that go off the top
    if(any(lr_rows < 1)){
      lr_rows[which(lr_rows < 1)] <- lr_rows[which(lr_rows < 1)]+nrow
    }
    ## Correct those that go off the bottom
    if(any(lr_rows > nrow)){
      lr_rows[which(lr_rows > nrow)] <- lr_rows[which(lr_rows > nrow)]-nrow
    }
    left_segment <- raster[lr_rows,left.col][,1]
    right_segment <- raster[lr_rows,right.col][,1]
    ## Get top and bottom
    tb_cols <- seq(cell_col-x,cell_col+x,1)
    ## Correct those that go off the left side
    if(any(tb_cols < 1)){
      tb_cols[which(tb_cols < 1)] <- tb_cols[which(tb_cols < 1)]+ncol
    }
    ## Correct those that go off the right side
    if(any(tb_cols > ncol)){
      tb_cols[which(tb_cols > ncol)] <- tb_cols[which(tb_cols > ncol)]-ncol
    }
    top_segment <- raster[top.row,tb_cols][,1]
    bottom_segment <- raster[bottom.row,tb_cols][,1]
    ## Concatenate
    ring <- c(top_segment,left_segment,bottom_segment,right_segment)
    ring <- unique(ring)
    return(ring)
  })
  return(output)
}
