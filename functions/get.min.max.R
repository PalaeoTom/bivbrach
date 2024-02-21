get.min.max <- function(data, nearest = 10){
  out <- c(round_any(max(c(data$max_ma, data$min_ma)), nearest, f = ceiling), round_any(min(c(data$max_ma, data$min_ma)), nearest, f = floor))
}
