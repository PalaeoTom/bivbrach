stage.checker <- function(input, stages, output = "names_only"){
  ## construct output
  out <- data.frame("raw" = input)
  converted <- ""
  out <- cbind(out, converted)
  ## sort stages by string length of stage names. Work from longest
  string_lengths <- nchar(stages$name)
  stages <- cbind(stages, string_lengths)
  stages <- stages[order(stages$string_lengths, decreasing = T),]
  ## Check for every stage name, longest to shortest. Will catch 'late', 'early' epithets and stop
  for(i in 1:nrow(stages)){
    ## Check for hits
    check <- str_detect(out[,"raw"],regex(stages[i,"name"], ignore_case = T))
    if(any(check)){
      ## update rows which match string and have blank converted cell
      out[intersect(which(check), which(out[,"converted"]=="")),"converted"] <- stages[i,"name"]
    }
  }
  if(output == "rich"){
    return(out)
  } else {
    if(output == "names_only"){
      return(out[,"converted"])
    } else {
      stop("specify output")
    }
  }
}
