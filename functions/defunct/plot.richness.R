plot.richness <- function(input.dir, input.pre, output.dir, output.pre, output.title, vars, vars.label, plotting.col, times.col, geo.scale, legend.position){
  ## get input and output strings
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".csv")
  output.dirs <- paste0(output.dir, "/", output.pre, "_", varStrings, ".pdf")
  ## get titles for plots
  title.combin <- expand.grid(vars.label)
  title.varStrings <- sapply(1:nrow(title.combin), function(x) paste(unlist(title.combin[x,]), collapse = ", "))
  titles <- paste0(output.title, ", " , title.varStrings)
  for(d in 1:length(input.dirs)){
    data <- suppressWarnings(tryCatch(read.csv(input.dirs[d]), error = function(e){}))
    ## if data is NULL, skip to next
    if(is.null(data)){
      next
    } else {
      ## if only one subsample, skip to next
      if(ncol(data) == 2){
        next
      } else {
        ## Add number of samples to title
        main.title <- paste0(titles[d], ", ", nrow(data), " subsamples")
        ## Define colour palette
        palette <- c()
        shape.palette <- c()
        for(i in 1:nrow(geo.scale)){
          count <- length(which(between(data[,times.col], left = geo.scale[,"t_age"][i], right = geo.scale[,"b_age"][i])))
          palette <- c(palette, rep(geo.scale[, "color"][i], count))
          shape.palette <- c(shape.palette, rep(geo.scale[, "shape"][i], count))
        }
        ## Define x and y axis limits
        x.mm <- c(0, round_any(max(data[,plotting.col[1]]), accuracy = 10, f = ceiling))
        y.mm <- c(0, round_any(max(data[,plotting.col[2]]), accuracy = 10, f = ceiling))
        ## get increments
        x.ax.inc <- round_any(x.mm[2]/4, accuracy = 10, f = ceiling)
        y.ax.inc <- round_any(y.mm[2]/4, accuracy = 10, f = ceiling)
        ## plot
        par(family = "Verdana")
        pdf(file = output.dirs[d])
        par(mar = c(5, 5, 2, 1), oma = c(0,0,0,0), lwd = 1, cex.axis = 1, cex.lab = 0.8, xpd = F)
        plot(x = NULL, y = NULL, ylim = y.mm, xlim = x.mm, axes = F, xlab = "", ylab = "", type = "n", yaxt = "n", xaxt = "n")
        points(x = data[,plotting.col[1]], y = data[,plotting.col[2]], col = palette, pch = shape.palette, cex = 1.2)
        axis(side = 2, las = 2, at = seq(y.mm[1], y.mm[2], y.ax.inc))
        axis(side = 1, las = 1, at = seq(x.mm[1], x.mm[2], x.ax.inc))
        abline(v = 0, col = "grey", lty = 2)
        abline(h = 0, col = "grey", lty = 2)
        box()
        legend(legend.position,
               title.font = 2,
               title = "Time periods",
               cex = 0.8,
               pt.cex = 1.5,
               pch = geo.scale[,"shape"],
               legend = geo.scale[,"name"],
               col = geo.scale[,"color"])
        par(xpd = T)
        text(x = x.mm[1]-(x.mm[2]*0.18), y = sum(y.mm)/2, paste0(plotting.col[2], " richness"), srt = 90)
        text(x = sum(x.mm)/2, y = y.mm[1]-(y.mm[2]*0.15), paste0(plotting.col[1], " richness"))
        title(main = main.title, cex.main = 0.8)
        dev.off()
      }
    }
  }
}
