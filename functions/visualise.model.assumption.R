visualise.model.assumption <- function(input.string, model.type, argument.strings, input.dir, output.dir, intervals = NULL){
  if(grepl("_g",input.string)){
    taxon <- "Genera,"
  } else {
    taxon <- "Species,"
  }
  if(grepl("200",input.string)){
    gCells <- "200km grid cells,"
  } else {
    gCells <- "100km grid cells,"
  }
  data.string <- paste(taxon, gCells)
  ## Read in model
  if(model.type == "full"){
    models <- readRDS(paste0(input.dir,"/",input.string,"_full_lmm_models.Rds"))
  } else {
    if(model.type == "simple"){
      models <- readRDS(paste0(input.dir,"/",input.string,"_simple_lmm_models.Rds"))
    } else {
      if(model.type == "median"){
        models <- readRDS(paste0(input.dir,"/",input.string,"_lmm_med_diff_models.Rds"))
      } else {
        if(model.type == "interval_RE_era"){
          models <- readRDS(paste0(input.dir,"/",input.string,"_lmm_eras_models.Rds"))
        } else {
          if(model.type == "interval_RE_PTME"){
            models <- readRDS(paste0(input.dir,"/",input.string,"_lmm_PTME_models.Rds"))
          } else {
            if(model.type == "interval_split_era"){
              model.P <- readRDS(paste0(input.dir,"/",input.string,"_lmm_eras_Paleozoic_models.Rds"))
              model.M <- readRDS(paste0(input.dir,"/",input.string,"_lmm_eras_Mesozoic_models.Rds"))
              model.C <- readRDS(paste0(input.dir,"/",input.string,"_lmm_eras_Cenozoic_models.Rds"))
              models <- list(model.P, model.M, model.C)
            } else {
              if(model.type == "interval_split_PTME"){
                model.pre <- readRDS(paste0(input.dir,"/",input.string,"_lmm_PTME_Pre-PTME_models.Rds"))
                model.post <- readRDS(paste0(input.dir,"/",input.string,"_lmm_PTME_Post-PTME_models.Rds"))
                models <- list(model.pre, model.post)
              } else {
                stop("check model.type")
              }
            }
          }
        }
      }
    }
  }
  if(model.type == "interval_split_era" || model.type == "interval_split_PTME"){
    for(s in 1:length(models)){
      if(length(models[[s]])>0){
        for(i in 1:length(models[[s]])){
              ## Get plot title
              if(model.type == "interval_split_era"){
                plot.title <- paste0(data.string, " ", argument.strings[which(argument.strings[,1] %in% names(models[[s]])[i]),2], ", ", intervals[s])
              } else {
                if(model.type == "interval_split_PTME"){
                  plot.title <- paste0(data.string, " ", argument.strings[which(argument.strings[,1] %in% names(models[[s]])[i]),2], ", ", intervals[s])
               } else {
                  stop("check model.type")
               }
                ## assemble data frame
                tdat <- data.frame(predicted=predict(models[[s]][[i]]), residual = residuals(models[[s]][[i]]))
                ## plot model assumption plot
                predVresid <- ggplot(tdat,aes(x=predicted,y=residual)) + geom_point() + geom_hline(yintercept=0, lty=3) +
                  ggtitle("Predicted values versus residuals")
                residHist <- ggplot(tdat,aes(x=residual)) + geom_histogram(bins=20, color="black") +
                  ggtitle("Residual distribution")
                QQ <- ggplot(tdat,aes(sample=residual)) + stat_qq() + stat_qq_line() +
                  ggtitle("QQ plot")
                ## define bottom row
                title <- ggdraw() + draw_label(plot.title, fontface = "bold")
                bottom_row <- plot_grid(residHist, QQ, ncol = 2, labels = c("B", "C"))
                final <- plot_grid(title, predVresid, bottom_row, nrow = 3, labels = c("", "A"), rel_heights = c(0.2, 1, 1))
                ## plot
                pdf(file = paste0(output.dir, "/", gsub(" ", "_", gsub(",", "", plot.title)), "_model_assumptions.pdf"))
                final
                dev.off()
          }
        }
      }
    }
  } else {
    if(length(models)>0){
      for(i in 1:length(models)){
          ## Get plot title
          if(model.type == "full"){
            plot.title <- paste0(data.string, " ", argument.strings[which(argument.strings[,1] %in% names(models)[i]),2], ", full model")
          } else {
            if(model.type == "simple"){
              plot.title <- paste0(data.string, " ", argument.strings[which(argument.strings[,1] %in% names(models)[i]),2], ", simple model")
            } else {
              if(model.type == "median"){
                plot.title <- paste0(data.string, " ", argument.strings[which(argument.strings[,1] %in% names(models)[i]),2], ", median richness")
              } else {
                if(model.type == "interval_RE_era"){
                  plot.title <- paste0(data.string, " ", argument.strings[which(argument.strings[,1] %in% names(models)[i]),2], ", eras as RE")
                } else {
                  if(model.type == "interval_RE_PTME"){
                    plot.title <- paste0(data.string, " ", argument.strings[which(argument.strings[,1] %in% names(models)[i]),2], ", PTME as RE")
                  } else {
                    stop("check model.type")
                  }
                }
              }
            }
          }
          ## assemble data frame
          tdat <- data.frame(predicted=predict(models[[i]]), residual = residuals(models[[i]]))
          ## plot model assumption plot
          predVresid <- ggplot(tdat,aes(x=predicted,y=residual)) + geom_point() + geom_hline(yintercept=0, lty=3) +
           ggtitle("Predicted values versus residuals")
          residHist <- ggplot(tdat,aes(x=residual)) + geom_histogram(bins=20, color="black") +
            ggtitle("Residual distribution")
          QQ <- ggplot(tdat,aes(sample=residual)) + stat_qq() + stat_qq_line() +
            ggtitle("QQ plot")
          ## define bottom row
          title <- ggdraw() + draw_label(plot.title, fontface = "bold")
          bottom_row <- plot_grid(residHist, QQ, ncol = 2, labels = c("B", "C"))
          final <- plot_grid(title, predVresid, bottom_row, nrow = 3, labels = c("", "A"), rel_heights = c(0.2, 1, 1))
          ## plot
          pdf(file = paste0(output.dir, "/", gsub(" ", "_", gsub(",", "", plot.title)), "_model_assumptions.pdf"))
          final
          dev.off()
      }
    }
  }
}
