round.timescale <- function(timescale, groupings, check = T){
  ## Round all to two digits
  for (group in 1:length(groupings)){
    bins <- groupings[[group]]
    digits <- c(3, 3, 3)[group]
    # round all up (so no 0-0 bins, and no overlap)
    for (i in bins){
      b <- timescale$b_age[i]
      t <- timescale$b_age[i+1]
      timescale$b_round[i] <- round.age(b, digits=digits, round_up=T)
      timescale$t_round[i] <- round.age(t, digits=digits, round_up=T)
    }
  }
  if(check){
    checker <- matrix(NA, nrow = length(2:nrow(timescale)), ncol = 2)
    checker[,1] <- timescale[,"name"][-1]
    for(i in 2:nrow(timescale)){
      if(timescale$t_round[i-1] == timescale$b_round[i]){
        checker[i-1,2] <- "equal to previous"
      } else {
        checker[i-1,2] <- "unequal"
      }
    }
    print(checker)
  }
  return(timescale)
}
