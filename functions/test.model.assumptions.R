test.model.assumptions <- function(model, mode = c("D", "Z", "O", "N", "C")){
  resmodel <- DHARMa::simulateResiduals(model, re.form = NULL)
  output <- list()
  cols <- c()
  values <- c()
  if(any(mode == "D")){
    disp <- DHARMa::testDispersion(resmodel, type = "DHARMa", plot = F)
    output <- c(output,list(disp))
    cols <- c(cols, "disp_ratio", "disp_p")
    values <- c(values, disp$statistic, disp$p.value)
  }
  if(any(mode == "Z")){
    zInf <- DHARMa::testZeroInflation(resmodel, plot = F)
    output <- c(output,list(zInf))
    cols <- c(cols, "zInf_ratio", "zInf_p")
    values <- c(values, zInf$statistic, zInf$p.value)
  }
  if(any(mode == "O")){
    outl <- DHARMa::testOutliers(resmodel, type = "bootstrap", plot = F)
    output <- c(output,list(outl))
    cols <- c(cols, "outl_exp", "outl_obs", "outl_p")
    values <- c(values, as.numeric(str_extract(names(outl$estimate), "\\d+.\\d+")), outl$estimate, outl$p.value)
  }
  if(any(mode == "N")){
    KS <- testUniformity(resmodel, plot = F)
    output <- c(output, list(KS))
    cols <- c(cols, "KS_stat", "KS_p")
    values <- c(values, KS$statistic, KS$p.value)

  }
  if(any(mode == "C")){
    VIF <- performance::check_collinearity(model)
    output <- c(output,list(VIF))
    cols <- c(cols, paste0("VIF_",VIF$Term))
    values <- c(values, VIF$VIF)
  }
  ## Combine values and cols into data.frame
  names(values) <- cols
  df <- t(data.frame(values))
  ## Plot, print, and return values
  plot(resmodel)
  print(output)
  return(df)
}
