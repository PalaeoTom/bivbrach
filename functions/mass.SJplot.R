mass.SJplot <- function(input.string, model.type, argument.strings, model.input.dir, rich.input.dir, output.dir, times.col, period.scale, era.scale, xy, time.cutoffs = NULL, min.sample = 20){
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
    models <- readRDS(paste0(model.input.dir,"/",input.string,"_full_lmm_models.Rds"))
  } else {
    if(model.type == "simple"){
      models <- readRDS(paste0(model.input.dir,"/",input.string,"_simple_lmm_models.Rds"))
    } else {
      if(model.type == "median"){
        models <- readRDS(paste0(model.input.dir,"/",input.string,"_lmm_med_diff_models.Rds"))
      } else {
        if(model.type == "interval_RE_era"){
          models <- readRDS(paste0(model.input.dir,"/",input.string,"_lmm_eras_models.Rds"))
        } else {
          if(model.type == "interval_RE_PTME"){
            models <- readRDS(paste0(model.input.dir,"/",input.string,"_lmm_PTME_models.Rds"))
          } else {
            if(model.type == "interval_split_era"){
              model.P <- readRDS(paste0(model.input.dir,"/",input.string,"_lmm_eras_Paleozoic_models.Rds"))
              model.M <- readRDS(paste0(model.input.dir,"/",input.string,"_lmm_eras_Mesozoic_models.Rds"))
              model.C <- readRDS(paste0(model.input.dir,"/",input.string,"_lmm_eras_Cenozoic_models.Rds"))
              models <- list(model.P, model.M, model.C)
            } else {
                if(model.type == "interval_split_PTME"){
                  model.pre <- readRDS(paste0(model.input.dir,"/",input.string,"_lmm_PTME_Pre-PTME_models.Rds"))
                  model.post <- readRDS(paste0(model.input.dir,"/",input.string,"_lmm_PTME_Post-PTME_models.Rds"))
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
  if(model.type == "median"){
    richness.list <- readRDS(paste0(model.input.dir,"/",input.string,"_lmm_med_diff_median_richness.Rds"))
  }
    if(model.type == "interval_split_era" || model.type == "interval_split_PTME"){
      for(s in 1:length(models)){
        if(length(models[[s]])>0){
          for(i in 1:length(models[[s]])){
            ## Read in richness data
            richness <- read.csv(paste0(rich.input.dir, "/", input.string, "_", names(models[[s]])[i], ".csv"), row.names = 1)
            ## Subsample down to period in question
            richness <- richness[intersect(which(richness[,"times"] >= time.cutoffs[s,"top"]),which(richness[,"times"] <= time.cutoffs[s,"bottom"])),]
            ## Only plot if 20 or more samples
            if(nrow(richness) >= min.sample){
              ## Also, only run if both samples have some variation (uninformative if all 0)
              if(length(unique(richness[,xy[1]])) > 1 && length(unique(richness[,xy[2]])) > 1){
              ## Use period.scale to assign period information
              period <- c()
              for(t in 1:nrow(period.scale)){
                count <- length(which(between(richness[,times.col], left = period.scale[,"t_age"][t], right = period.scale[,"b_age"][t])))
                if(count > 0){
                  period <- c(period, rep(period.scale[,"name"][t], count))
                }
              }
              era <- c()
              for(t in 1:nrow(era.scale)){
                count <- length(which(between(richness[,times.col], left = era.scale[,"t_age"][t], right = era.scale[,"b_age"][t])))
                if(count > 0){
                  era <- c(era, rep(era.scale[,"name"][t], count))
                }
              }
              richness <- cbind(richness, period, era)
              ## Get colour vector
              point.col <- period.scale[,"color"]
              names(point.col) <- period.scale[,"name"]
              ## Get shape vector
              point.shape <- era.scale[,"shape"]
              names(point.shape) <- era.scale[,"name"]
              ## Get shape vector for data
              era.legend <- period.scale[,"shape"]
              era.legend <- era.legend[period.scale[,"name"] %in% unique(richness[,"period"])]
              ## Get plot title
              if(model.type == "interval_split_era"){
                plot.title <- paste0(data.string, " ", argument.strings[which(argument.strings[,1] %in% names(models[[s]])[i]),2], ", ", rownames(time.cutoffs)[s])
              } else {
                if(model.type == "interval_split_PTME"){
                  plot.title <- paste0(data.string, " ", argument.strings[which(argument.strings[,1] %in% names(models[[s]])[i]),2], ", ", rownames(time.cutoffs)[s])
                } else {
                  stop("check model.type")
                  }
              }
              ## define plot data frame
              line.df <- get_model_data(models[[s]][[i]], type = "pred", terms = xy[1])
              ## Export model summary
              sink(file = paste0(output.dir, "/", gsub(" ", "_", gsub(",", "", plot.title)), "_model_summary.txt"))
              print(summary(models[[s]][[i]]))
              sink()
              ## basic scatter plot
              scatter <- ggplot() +
                xlab("Bivalve richness") +
                ylab("Brachiopod richness") +
                labs(color = "Period") +
                ggtitle(plot.title) +
                geom_point(data = richness, aes(x = Bivalvia, y = Brachiopoda, color = period, shape = era)) +
                scale_color_manual(breaks = unique(richness[,"period"]), values = point.col) +
                scale_shape_manual(values = point.shape) +
                geom_line(data = line.df, aes(x = x, y = predicted)) +
                geom_ribbon(data = line.df, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1)+
                scale_x_continuous(expand = c(0,1)) +
                scale_y_continuous(expand = c(0,1)) +
                geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
                geom_vline(xintercept = 0, colour = "grey", linetype = "dashed") +
                guides(shape = "none",
                       color = guide_legend(override.aes = list(shape = era.legend))) +
                theme(text = element_text(family = "Helvetica"),,
                      title = element_text(size = 12),
                      axis.text = element_text(size = 11),
                      axis.title = element_text(size = 12),
                      legend.title = element_text(size = 12),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "black"),
                      legend.position = "bottom",
                      legend.text = element_text(size = 10),
                      legend.key.size = unit(10,"point"))
              ## Coefficient plot
              coeff <- plot_model(models[[s]][[i]], vline = "grey", show.values = T, title = "", value.offset = 0.2) +
                theme(text = element_text(family = "Helvetica"),,
                      title = element_text(size = 12),
                      axis.text = element_text(size = 11),
                      axis.title = element_text(size = 12),
                      legend.title = element_text(size = 12),
                      axis.line = element_line(colour = "black"))
              ## define title
              title <- ggdraw() + draw_label(plot.title, fontface = "bold")
              ## create grid
              bottom_row <- plot_grid(scatter, coeff, ncol = 2, labels = c("A", "B"), rel_widths = c(1.2,1))
              final <- plot_grid(title, bottom_row, nrow = 3, labels = c(""), rel_heights = c(0.2, 1))
              final
              ## plot
              pdf(file = paste0(output.dir, "/", gsub(" ", "_", gsub(",", "", plot.title)), ".pdf"))
              print(final)
              dev.off()
              }
            }
          }
        }
      }
    } else {
      if(length(models)>0){
      for(i in 1:length(models)){
        ## Read in richness data
        if(model.type == "median"){
          richness <- richness.list[[i]]
        } else {
          richness <- read.csv(paste0(rich.input.dir, "/", input.string, "_", names(models)[i], ".csv"))
        }
        ## Only plot if 20 or more samples
        if(nrow(richness) >= min.sample){
        ## Use period.scale to assign period information
        period <- c()
        for(t in 1:nrow(period.scale)){
          count <- length(which(between(richness[,times.col], left = period.scale[,"t_age"][t], right = period.scale[,"b_age"][t])))
          if(count > 0){
            period <- c(period, rep(period.scale[,"name"][t], count))
          }
        }
        era <- c()
        for(t in 1:nrow(era.scale)){
          count <- length(which(between(richness[,times.col], left = era.scale[,"t_age"][t], right = era.scale[,"b_age"][t])))
          if(count > 0){
            era <- c(era, rep(era.scale[,"name"][t], count))
          }
        }
        richness <- cbind(richness, period, era)
        ## Get colour vector
        point.col <- period.scale[,"color"]
        names(point.col) <- period.scale[,"name"]
        ## Get shape vector
        point.shape <- era.scale[,"shape"]
        names(point.shape) <- era.scale[,"name"]
        ## Get shape vector for data
        era.legend <- period.scale[,"shape"]
        era.legend <- era.legend[period.scale[,"name"] %in% unique(richness[,"period"])]
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
        ## define plot data frame
        line.df <- get_model_data(models[[i]], type = "pred", terms = xy[1])
        ## Export model summary
        sink(file = paste0(output.dir, "/", gsub(" ", "_", gsub(",", "", plot.title)), "_model_summary.txt"))
        print(summary(models[[i]]))
        sink()
        ## basic scatter plot
        scatter <- ggplot() +
          xlab("Bivalve richness") +
          ylab("Brachiopod richness") +
          labs(color = "Period") +
          geom_point(data = richness, aes(x = Bivalvia, y = Brachiopoda, color = period, shape = era)) +
          scale_color_manual(breaks = unique(richness[,"period"]), values = point.col) +
          scale_shape_manual(values = point.shape) +
          geom_line(data = line.df, aes(x = x, y = predicted)) +
          geom_ribbon(data = line.df, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1)+
          scale_x_continuous(expand = c(0,1)) +
          scale_y_continuous(expand = c(0,1)) +
          geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
          geom_vline(xintercept = 0, colour = "grey", linetype = "dashed") +
          guides(shape = "none",
                 color = guide_legend(override.aes = list(shape = era.legend))) +
          theme(text = element_text(family = "Helvetica"),,
                title = element_text(size = 12),
                axis.text = element_text(size = 11),
                axis.title = element_text(size = 12),
                legend.title = element_text(size = 12),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.position = "bottom",
                legend.text = element_text(size = 9),
                legend.key.size = unit(10,"point"))
        ## Coefficient plot
        coeff <- plot_model(models[[i]], vline = "grey", show.values = T, title = "", value.offset = 0.2) +
          theme(text = element_text(family = "Helvetica"),,
                title = element_text(size = 12),
                axis.text = element_text(size = 11),
                axis.title = element_text(size = 12),
                legend.title = element_text(size = 12),
                axis.line = element_line(colour = "black"))
        ## define title
        title <- ggdraw() + draw_label(plot.title, fontface = "bold")
        ## create grid
        bottom_row <- plot_grid(scatter, coeff, ncol = 2, labels = c("A", "B"), rel_widths = c(1.2,1))
        final <- plot_grid(title, bottom_row, nrow = 3, labels = c(""), rel_heights = c(0.2, 1))
        final
        ## plot
        pdf(file = paste0(output.dir, "/", gsub(" ", "_", gsub(",", "", plot.title)), ".pdf"))
        print(final)
        dev.off()
        }
      }
    }
  }
}
