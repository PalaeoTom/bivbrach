plot.spatSubThroughTime <- function(input.strings, output.dir, output.strings, strat.data, time.data, titles, legend.position, legend.labels, y.axis.inc, x.axis.inc, line.pal, line.type.pal){
  for(i in 1:length(input.strings)){
    ## get data
    data <- eval(parse(text = input.strings[i]))
    ## Get constraints of axes
    y.mm <- c(0, max(data))
    x.mm <- c(0, max(strat.data$b_age))
    ## plot
    par(family = "Verdana")
    pdf(file = paste0(output.dir, "/", output.strings[i], "_spatSubThroughTime.pdf"))
    par(mar = c(5, 5, 1, 1), oma = c(0,0,0,0), lwd = 1, cex.axis = 1, cex.lab = 0.8, xpd = T)
    plot(x = NULL, y = NULL, ylim = c(y.mm[1]-y.mm[2]*0.05, y.mm[2]), xlim = rev(x.mm), xaxs = "i", yaxs = "i", axes = F, xlab = "", ylab = "", type = "n", yaxt = "n", xaxt = "n")
    add.geo.scale(y.bottom = y.mm[1], scale.bottom = y.mm[1]-y.mm[2]*0.05, time.data = time.data, strat.data = strat.data, drop.R.label = T)
    shade.time(y.top = max(y.mm), y.bottom = min(y.mm), strat.data = strat.data)
    for(d in 1:nrow(data)){
      lines(x = time.data, y = data[d,], col = line.pal[d], lty = line.type.pal[d], lwd = 1.5)
    }
    axis(side = 2, las = 2, at = seq(y.mm[1], y.mm[2], y.ax.inc))
    axis(side = 1, las = 1, at = seq(x.mm[1], x.mm[2], x.ax.inc))
    legend(legend.position[i],
           title = titles[i],
           title.font = 2,
           cex = 0.8,
           lty = line.type.pal,
           lwd = 2,
           legend = legend.labels,
           col = line.pal)
    box()
    text(x = x.mm[2]+(x.mm[2]*0.1), y = sum(y.mm)/2, "Number of viable spatial subsamples", srt = 90)
    text(x = sum(x.mm)/2, y = y.mm[1]-(y.mm[2]*0.15), "Time (Ma)")
    dev.off()
  }
}
