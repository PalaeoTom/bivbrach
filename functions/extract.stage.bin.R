extract.stage.bin <- function(data, MA.start, MA.end, included){
  # get rows of occurrences that originate in interval
  orig <- union(which(between(data[,"max_ma"], MA.end, MA.start)),which(data$early_interval %in% included))
  # get rows of occurrences that terminate in interval
  term <- union(which(between(data[,"min_ma"], MA.end, MA.start)),which(data$late_interval %in% included))
  # get all entries that fit within interval
  lifers <- intersect(orig,term)
  # Get data
  output <- data[lifers,]
}
