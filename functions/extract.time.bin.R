extract.time.bin <- function(data, MA.start, MA.end){
  # get all occurrences that originate in interval
  orig <- data[which(between(data[,"max_ma"], MA.end, MA.start)),]
  # get all occurrences that terminate in interval
  term <- data[which(between(data[,"min_ma"], MA.end, MA.start)),]
  # get all entries that fit within interval
  raw <- c(which(data[,"max_ma"] > MA.start),which(data[,"min_ma"] < MA.end))
  span <- data[raw[duplicated(raw)],]
  # concatenate
  output <- rbind(orig, term, span)
  return(output)
}
