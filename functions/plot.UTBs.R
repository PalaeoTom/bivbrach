plot.UTBs <- function(data, xlab.inc = 5, output.dir, output.name){
  home <- getwd()
  ylim <- c(0,round_any(max(sapply(1:length(data), function(r) max(data[[r]][,"usableTimeBins"]))), 10, ceiling))
  ylabels <- seq(0, ylim[2], xlab.inc)
  ylabels <- ylabels[-length(ylabels)]
  xlabels <- c(paste0("Area ", overlapThresholds*100, "%"), paste0("Sites ", overlapThresholds*100, "%"))
  palette <- c(brewer.pal(5, "Blues"), brewer.pal(5, "Greens"))
  setwd(output.dir)
  ## make figure
  par(family = "Verdana")
  pdf(paste0(output.name, "_usableTimeBins.pdf"))
  layout(matrix(1:20, ncol = 5), widths = c(1.6, 1, 1, 1, 1.25), heights = c(1, 1, 1, 1.3))
  ## Column 1
  par(mar = c(0, 5, 2, 0), oma = c(0,2,0,0), lwd = 1, cex.axis = 0.75, xpd = T)
  barplot(height = data[[1]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = xlabels, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  text(x = 0, y = -5, "Radius 100km")
  par(mar = c(0, 5, 0, 0))
  barplot(height = data[[2]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = xlabels, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  barplot(height = data[[3]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = xlabels, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  par(mar = c(2,5,0,0))
  barplot(height = data[[4]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = xlabels, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  axis(side = 1, las = 1, at = ylabels, cex = 0.5)
  ## Column 2
  par(mar = c(0, 0, 2, 0))
  barplot(height = data[[5]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  par(mar = c(0, 0, 0, 0))
  barplot(height = data[[6]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  barplot(height = data[[7]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  par(mar = c(2,0,0,0))
  barplot(height = data[[8]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  axis(side = 1, las = 1, at = ylabels, cex = 0.5)
  ## Column 3
  par(mar = c(0, 0, 2, 0))
  barplot(height = data[[9]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  par(mar = c(0, 0, 0, 0))
  barplot(height = data[[10]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  barplot(height = data[[11]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  par(mar = c(2,0,0,0))
  barplot(height = data[[12]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  axis(side = 1, las = 1, at = ylabels, cex = 0.5)
  ## Column 4
  par(mar = c(0, 0, 2, 0))
  barplot(height = data[[13]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  par(mar = c(0, 0, 0, 0))
  barplot(height = data[[14]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  barplot(height = data[[15]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  par(mar = c(2,0,0,0))
  barplot(height = data[[16]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  axis(side = 1, las = 1, at = ylabels, cex = 0.5)
  ## Column 5
  par(mar = c(0, 0, 2, 2))
  barplot(height = data[[17]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  par(mar = c(0, 0, 0, 2))
  barplot(height = data[[18]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  barplot(height = data[[19]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  par(mar = c(2,0,0,2))
  barplot(height = data[[20]][,"usableTimeBins"], xlim = ylim, horiz = T, xaxs = "i", yaxs = "i", axes = F, names.arg = NULL, las = 2, space = 0,
          xlab = "", width = 0.2, col = palette, border = palette)
  box()
  axis(side = 1, las = 1, at = ylabels, cex = 0.5)
  dev.off()
  setwd(home)
}
