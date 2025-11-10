rarefaction_curve <- function(occs_samp, iter) {
  N <- length(occs_samp)
  occs_f <- as.integer(factor(occs_samp))
  mean_vec <- numeric(N)
  for (i in seq_len(iter)) {
    sampleX <- sample(occs_f, N, replace = FALSE)
    mean_vec <- mean_vec + cumsum(!duplicated(sampleX))
  }
  mean_vec / iter
}
