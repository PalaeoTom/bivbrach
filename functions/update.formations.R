update.formations <- function(data, columns, key){
  if(length(columns)>1){
    for(c in columns){
      for(r in 1:nrow(key)){
        if(any(data[,c]==key[r,"verbatim_name"])){
          data[which(data[,c]==key[r,"verbatim_name"]),c] <- key[r,"updated_name"]
        }
      }
    }
  } else {
      for(r in 1:nrow(key)){
        if(any(data[,columns]==key[r,"verbatim_name"])){
          data[which(data[,columns]==key[r,"verbatim_name"]),columns] <- key[r,"updated_name"]
        }
      }
  }
  return(data)
}
