clean.stage.names <- function(data, columns, stages){
  for(i in columns){
    ## strip accents
    data[,i] <- stringi::stri_trans_general(data[,i], "Latin-ASCII")
    ## now swap out trailing ien for ian
    data[,i] <- str_replace(data[,i], pattern = "ien", replacement = "ian")
    ## Fix Pliansbachian to Pliensbachian
    data[,i] <- str_replace(data[,i], pattern = "Pliansbachian", replacement = "Pliensbachian")
  }
  ## Return
  return(data)
}
