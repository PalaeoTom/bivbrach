test.model.assumptions <- function(model, mode = c("D", "Z", "O", "N", "C")){
  resmodel <- simulateResiduals(model, re.form = NULL)
  output <- list()
  if(any(mode == "D")){
  testDispersion(resmodel, type = "DHARMa")
  output <- c(output,list(testDispersion(resmodel, type = "DHARMa")))
  }
  if(any(mode == "Z")){
  testZeroInflation(resmodel)
  output <- c(output,list(testZeroInflation(resmodel)))
  }
  if(any(mode == "O")){
  testOutliers(resmodel, type = "bootstrap")
  output <- c(output,list(testOutliers(resmodel, type = "bootstrap")))
  }
  if(any(mode == "N")){
  plot(resmodel)
  }
  if(any(mode == "C")){
  check_collinearity(model)
  output <- c(output,list(check_collinearity(model)))
  }
  print(output)
}
