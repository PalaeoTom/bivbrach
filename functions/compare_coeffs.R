compare_coeffs <- function(emp, sim, coeff, p_value_and_coeff, threshold = 0.05){
  emp_coeff <- emp[which(emp[,"term"] == coeff),"estimate"]
  emp_pValue <- emp[which(emp[,"term"] == coeff),"p.value"]
  ## define column names
  coeffN <- paste0(coeff, "_coefficients")
  pValueN <- paste0(coeff, "_pValues")
  ## define tracker
  tracker <- c()
  for(i in 1:nrow(sim)){
    if(p_value_and_coeff){
      ## If emp_coeff positive or negative
      if(sign(emp_coeff) == -1){
        ## Look for significance + equal or smaller
        if((sim[i,coeffN] <= emp_coeff) && (sim[i,pValueN] < threshold)){
          tracker <- c(tracker, TRUE)
        } else {
          tracker <- c(tracker, FALSE)
        }
      } else {
        if(sign(emp_coeff) == 1){
          ## Positive. Look for significance plus equal or greater
          if((sim[i,coeffN] >= emp_coeff) && (sim[i,pValueN] < threshold)){
            tracker <- c(tracker, TRUE)
          } else {
            tracker <- c(tracker, FALSE)
          }
        } else {
          ## Empirical coefficient is 0. Note comparison as NA
          stop("empirical coefficient being compared is 0. Why bother with this test?")
        }
      }
    } else {
      ## Check polarity and p-value
      if((sign(emp_coeff) == sign(sim[i,coeffN])) && (sim[i,pValueN] <= threshold)){
        tracker <- c(tracker, TRUE)
      } else {
        tracker <- c(tracker, FALSE)
      }
    }
  }
  return(tracker)
}
