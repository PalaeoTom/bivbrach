extract.time.bin <- function(data, MA.start, MA.end){
  # get rows of occurrences that originate in interval
  orig <- which(between(data[,"max_ma"], MA.end, MA.start))
  # get rows of occurrences that terminate in interval
  term <- which(between(data[,"min_ma"], MA.end, MA.start))
  # get all entries that fit within interval
  lifers <- intersect(orig,term)
  # Get data
  output <- data[lifers,]
}
