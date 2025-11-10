generate_coeff_histogram <- function(emp, sim, coeff, xlim = c(-2,2)){
  ## Get empirical coefficient
  emp_coeff <- emp[which(emp[,"term"] == coeff),"estimate"]
  ## define column name for simulated data
  coeffN <- paste0(coeff, "_coefficients")
  ## Plot histogram
  hist(sim[,coeffN], xlim = xlim, main = paste0("Null model log odds versus empirical for predictor '", coeff, "'"), xlab = "Log odds", yaxs = "i", xaxs = "i")
  ## Plot empirical value
  abline(v = emp[which(emp$term == coeff),"estimate"], col = "red")
  ## Add legend
  legend("topleft", legend = c("Empirical"), lty = 1, col = "red")
}
