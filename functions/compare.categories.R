compare.categories <- function(input.pre, output.pre, output.dir, covariate, data.strings){
  combin <- expand.grid(vars)
  varStrings <- sapply(1:nrow(combin), function(x) paste(unlist(combin[x,]), collapse = "_"))
  input.dirs <- paste0(input.dir, "/", input.pre, "_", varStrings, ".csv")
  for(i in 1:length(input.dirs)){
    #print(paste0("i = ", i))
    ## Read in richness
    data <- suppressWarnings(tryCatch(read.csv(input.dirs[i], row.names = 1, stringsAsFactors = T), error = function(e){}))
    ## if data not null
    if(!is.null(data)){
      ## Split into bivalves and brachiopods
      dataL <- lapply(1:length(data.strings), function(x) data[,c(data.strings[x],covariate)])
      ## Perform anova and plot boxplot for each categorical variable
      for(d in 1:length(dataL)){
        #print(paste0("d = ", d))
        if(length(unique(dataL[[d]][,covariate]))>1 && length(unique(dataL[[d]][,1]))>1){
          ## ensure richness is numeric
          dataL[[d]][,data.strings[d]] <- as.numeric(dataL[[d]][,data.strings[d]])
          ## run analysis of variance
          if(data.strings[d] == "Bivalvia"){
            if(covariate == "sampEnv"){
              ## Set mix as reference
              dataL[[d]][,covariate] <- relevel(dataL[[d]][,covariate], ref = "mix")
              ## Generate model
              model <- lm(Bivalvia ~ sampEnv, data = dataL[[d]])
              ## Plot model assumptipons
              pdf(file = paste0(output.dir, "/", "Bivalvia_sampEnv_", varStrings[i], "_model_assumptions.pdf"))
              par(mfrow = c(2,2))
              plot(model)
              par(mfrow = c(1,1))
              dev.off()
              ## report model
              sink(file = paste0(output.dir, "/", "Bivalvia_sampEnv_", varStrings[i], "_model_summary.txt"))
              print(summary(model))
              sink()
              ## plot box plot
              pdf(file = paste0(output.dir, "/", "Bivalvia_sampEnv_", varStrings[i], "_boxplot.pdf"))
              plot_grpfrq(dataL[[d]]$Bivalvia, var.grp = dataL[[d]]$sampEnv, type = "box")
              dev.off()
            } else {
              if(covariate == "sampLith"){
                ## Set mix as reference
                dataL[[d]][,covariate] <- relevel(dataL[[d]][,covariate], ref = "mix")
                ## Generate model
                model <- lm(Bivalvia ~ sampLith, data = dataL[[d]])
                ## Plot model assumptipons
                pdf(file = paste0(output.dir, "/", "Bivalvia_sampLith_", varStrings[i], "_model_assumptions.pdf"))
                par(mfrow = c(2,2))
                plot(model)
                par(mfrow = c(1,1))
                dev.off()
                ## report model
                sink(file = paste0(output.dir, "/", "Bivalvia_sampLith_", varStrings[i], "_model_summary.txt"))
                print(summary(model))
                sink()
                ## plot box plot
                pdf(file = paste0(output.dir, "/", "Bivalvia_sampLith_", varStrings[i], "_boxplot.pdf"))
                plot_grpfrq(dataL[[d]]$Bivalvia, var.grp = dataL[[d]]$sampLith, type = "box")
                dev.off()
              } else {
                if(covariate == "sampReef"){
                  ## Set mix as reference
                  dataL[[d]][,covariate] <- relevel(dataL[[d]][,covariate], ref = "mix")
                  ## Generate model
                  model <- lm(Bivalvia ~ sampReef, data = dataL[[d]])
                  ## Plot model assumptipons
                  pdf(file = paste0(output.dir, "/", "Bivalvia_sampReef_", varStrings[i], "_model_assumptions.pdf"))
                  par(mfrow = c(2,2))
                  plot(model)
                  par(mfrow = c(1,1))
                  dev.off()
                  ## report model
                  sink(file = paste0(output.dir, "/", "Bivalvia_sampReef_", varStrings[i], "_model_summary.txt"))
                  print(summary(model))
                  sink()
                  ## plot box plot
                  pdf(file = paste0(output.dir, "/", "Bivalvia_sampReef_", varStrings[i], "_boxplot.pdf"))
                  plot_grpfrq(dataL[[d]]$Bivalvia, var.grp = dataL[[d]]$sampReef, type = "box")
                  dev.off()
                } else {
                  if(covariate == "sampLat"){
                    ## ensure latitude
                    dataL[[d]][,covariate] <- as.numeric(dataL[[d]][,covariate])
                    ## Generate model
                    model <- lm(Bivalvia ~ sampLat, data = dataL[[d]])
                    ## Plot residuals
                    pdf(file = paste0(output.dir, "/", "Bivalvia_sampLat_", varStrings[i], "_residuals.pdf"))
                    hist(model$residuals)
                    dev.off()
                    ## report model
                    sink(file = paste0(output.dir, "/", "Bivalvia_sampLat_", varStrings[i], "_model_summary.txt"))
                    print(summary(model))
                    sink()
                    ## plot line plot
                    pdf(file = paste0(output.dir, "/", "Bivalvia_sampLat_", varStrings[i], "_lineplot.pdf"))
                    plot(y = dataL[[d]]$Bivalvia, x = dataL[[d]]$sampLat, xlab = "sampLat", ylab = "Bivalvia richness")
                    dev.off()
                  } else {
                    stop("check covariate")
                  }
                }
              }
            }
          } else {
            if(data.strings[d] == "Brachiopoda"){
              if(covariate == "sampEnv"){
                ## Set mix as reference
                dataL[[d]][,covariate] <- relevel(dataL[[d]][,covariate], ref = "mix")
                ## Generate model
                model <- lm(Brachiopoda ~ sampEnv, data = dataL[[d]])
                ## Plot model assumptipons
                pdf(file = paste0(output.dir, "/", "Brachiopoda_sampEnv_", varStrings[i], "_model_assumptions.pdf"))
                par(mfrow = c(2,2))
                plot(model)
                par(mfrow = c(1,1))
                dev.off()
                ## report model
                sink(file = paste0(output.dir, "/", "Brachiopoda_sampEnv_", varStrings[i], "_model_summary.txt"))
                print(summary(model))
                sink()
                ## plot box plot
                pdf(file = paste0(output.dir, "/", "Brachiopoda_sampEnv_", varStrings[i], "_boxplot.pdf"))
                plot_grpfrq(dataL[[d]]$Brachiopoda, var.grp = dataL[[d]]$sampEnv, type = "box")
                dev.off()
              } else {
                if(covariate == "sampLith"){
                  ## Set mix as reference
                  dataL[[d]][,covariate] <- relevel(dataL[[d]][,covariate], ref = "mix")
                  ## Generate model
                  model <- lm(Brachiopoda ~ sampLith, data = dataL[[d]])
                  ## Plot model assumptipons
                  pdf(file = paste0(output.dir, "/", "Brachiopoda_sampLith_", varStrings[i], "_model_assumptions.pdf"))
                  par(mfrow = c(2,2))
                  plot(model)
                  par(mfrow = c(1,1))
                  dev.off()
                  ## report model
                  sink(file = paste0(output.dir, "/", "Brachiopoda_sampLith_", varStrings[i], "_model_summary.txt"))
                  print(summary(model))
                  sink()
                  ## plot box plot
                  pdf(file = paste0(output.dir, "/", "Brachiopoda_sampLith_", varStrings[i], "_boxplot.pdf"))
                  plot_grpfrq(dataL[[d]]$Brachiopoda, var.grp = dataL[[d]]$sampLith, type = "box")
                  dev.off()
                } else {
                  if(covariate == "sampReef"){
                    ## Set mix as reference
                    dataL[[d]][,covariate] <- relevel(dataL[[d]][,covariate], ref = "mix")
                    ## Generate model
                    model <- lm(Brachiopoda ~ sampReef, data = dataL[[d]])
                    ## Plot model assumptipons
                    pdf(file = paste0(output.dir, "/", "Brachiopoda_sampReef_", varStrings[i], "_model_assumptions.pdf"))
                    par(mfrow = c(2,2))
                    plot(model)
                    par(mfrow = c(1,1))
                    dev.off()
                    ## report model
                    sink(file = paste0(output.dir, "/", "Brachiopoda_sampReef_", varStrings[i], "_model_summary.txt"))
                    print(summary(model))
                    sink()
                    ## plot box plot
                    pdf(file = paste0(output.dir, "/", "Brachiopoda_sampReef_", varStrings[i], "_boxplot.pdf"))
                    plot_grpfrq(dataL[[d]]$Brachiopoda, var.grp = dataL[[d]]$sampReef, type = "box")
                    dev.off()
                  } else {
                    if(covariate == "sampLat"){
                      ## ensure latitude
                      dataL[[d]][,covariate] <- as.numeric(dataL[[d]][,covariate])
                      ## Generate model
                      model <- lm(Brachiopoda ~ sampLat, data = dataL[[d]])
                      ## Plot residuals
                      pdf(file = paste0(output.dir, "/", "Brachiopoda_sampLat_", varStrings[i], "_residuals.pdf"))
                      hist(model$residuals)
                      dev.off()
                      ## report model
                      sink(file = paste0(output.dir, "/", "Brachiopoda_sampLat_", varStrings[i], "_model_summary.txt"))
                      print(summary(model))
                      sink()
                      ## plot line plot
                      pdf(file = paste0(output.dir, "/", "Brachiopoda_sampLat_", varStrings[i], "_lineplot.pdf"))
                      plot(y = dataL[[d]]$Brachiopoda, x = dataL[[d]]$sampLat, xlab = "sampLat", ylab = "Brachiopoda richness")
                      dev.off()
                    } else {
                      stop("check covariate")
                    }
                  }
                }
              }
            } else {
              stop("check data.strings")
            }
          }
        }
      }
    }
  }
}
