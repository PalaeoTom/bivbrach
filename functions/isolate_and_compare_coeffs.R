isolate_and_compare_coeffs <- function(simModels, mainModel, coeffs, fig.export.dir, data.export.dir, figure.name, data.name, plot.title, plot.limits, visualsRef){
  ## Get coefficients for simModels
  values <- isolate.coeffs(simModels)
  ## If any have NA values, find and drop from values and simModels
  if(any(apply(values, 1, function(x) any(is.na(x))))){
    droppers <- which(apply(values, 1, function(x) any(is.na(x))))
    values <- values[-droppers,]
    simModels[droppers] <- NULL
  }
  ## Get coefficients for original model
  orig_v <- get_model_data(mainModel, type = "est", transform = NULL)
  ## Create output for comparisons
  output <- data.frame(matrix(NA, nrow = 3, ncol = length(coeffs)))
  colnames(output) <- coeffs
  rownames(output) <- c("n.comp", "n.coeffEOG", "n.coeffEOG.pValueEOG")
  ## For each coefficient
  for(c in 1:length(coeffs)){
    ## Get number of simulated models with coefficients
    output[1,c] <- length(which(!is.na(simModels)))
    ## Get number of simulations where coefficients meet or exceed empirical
    output[2,c] <- length(which(compare_coeffs(orig_v, values, coeff = coeffs[c], p_value_and_coeff = F)))
    ## Get number of simulations where coefficients meet/exceed and p-value does as well!
    output[3,c] <- length(which(compare_coeffs(orig_v, values, coeff = coeffs[c], p_value_and_coeff = T)))
  }
  ## Export output as csv and coefficients
  write.csv(values, paste0(data.export.dir, "/", data.name, "_raw.csv"))
  write.csv(output, paste0(data.export.dir, "/", data.name, "_summary.csv"))
  ## Now to generate figure first, if any rows = NA, drop them
  if(any(apply(values, 1, function(x) all(is.na(x))))){
    values <- values[-which(apply(values, 1, function(x) all(is.na(x)))),]
  }
  ## Rearrange visualsRef and orig_v to match order
  visualsRef <- visualsRef[match(coeffs, visualsRef[,"term"]),]
  orig_v <- orig_v[match(coeffs, orig_v[,"term"]),]
  plot.limits <- plot.limits[match(coeffs, names(plot.limits))]
  ## Generate plots
  plots <- lapply(1:length(coeffs), function(x){
    p <- ggplot(values, aes(x = !!sym(paste0(coeffs[x], "_coefficients")))) +
      geom_histogram(binwidth = 0.01, colour = visualsRef[x,"colour"]) +
      scale_x_continuous(expand = c(0,0), limits = plot.limits[[x]]) +
      scale_y_continuous(expand = c(0,0)) +
      geom_vline(xintercept = orig_v[x,"estimate"], linetype = "dashed", linewidth = 1, colour = "red") +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
      ggtitle(label = visualsRef[x,"labels"]) +
      xlab("Log-Odds") +
      ylab("Frequency") +
      theme(text = element_text(family = "Helvetica"),
            title = element_text(size = 7),
            axis.text = element_text(size = 8),
            axis.title = element_text(size = 8),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "none")
  })
  ## Plot as grid
  grid <- plot_grid(plotlist = plots, labels = LETTERS[1:length(coeffs)], label_size = 8)
  title <- ggdraw() + draw_label(plot.title, fontface='bold', size = 10)
  fig.out <- plot_grid(title, grid, ncol=1, rel_heights=c(0.075, 1)) # rel_heights values control title margins
  ## Check
  fig.out
  ## Save as PDF
  pdf(paste0(fig.export.dir, "/", figure.name, "_simVersusEmp.pdf"))
  print(fig.out)
  dev.off()
}
