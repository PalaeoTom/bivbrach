get_intercepts <- function(data, angle){
  ## Get cells
  cells <- unique(colnames(data))
  ## for each cell
  interceptx <- c()
  intercepty <- c()
  c = 2
  for(c in cells){
    ## Get cell
    cell <- data[!is.na(data[,c]),c]
    ## Get sample count
    count <- seq(1,length(cell),1)
    ## Get line slope
    slope <- tan(angle*(pi/(180)))
    ## Get line values
    line <- get_line_values(slope, x = length(cell), y = cell[length(cell)])
    ## Drop final entry for ease
    cell <- cell[-length(cell)]
    line <- line[-length(line)]
    ## If all are greater, then no true intercept
    if(all(line > cell)){
      interceptx <- c(interceptx, NA)
      intercepty <- c(intercepty, NA)
    } else {
      ## If all are less, intercept is at x = 0 and y-intercept
      if(all(line < cell)){
        interceptx <- c(interceptx, 0)
        intercepty <- c(intercepty, (cell[length(cell)] - (slope*(length(cell)))))
      } else {
        ## Get differences
        diffs <- cell-line
        ## if any are 0, record that exact value
        if(any(diffs == 0)){
          interceptx <- c(interceptx, which(diffs == 0))
          intercepty <- c(intercepty, cell[which(diffs == 0)])
        } else {
          ## Find transition differences index
          transition <- c(((which(+(c(0, diff(sign(diffs))) != 0) == 1)[1])-1),which(+(c(0, diff(sign(diffs))) != 0) == 1)[1])
          ## Get xvalue
          curvey <- cell[transition]
          ## Get rounded average
          interceptx <- c(interceptx, mean(transition))
          intercepty <- c(intercepty, mean(curvey))
        }
      }
    }
  }
  output <- data.frame(cbind("cells" = cells, "x_value" = interceptx, "y_value" = intercepty))
  return(output)
}
