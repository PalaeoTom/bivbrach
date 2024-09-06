mass.SJplot <- function(input.string, model.type, argument.strings, model.input.dir, rich.input.dir, output.dir, times.col, period.scale, era.scale, xy){
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
      stop("check model.type")
    }
  }
  if(length(models)>0){
  for(i in 1:length(models)){
    ## Read in richness data
    richness <- read.csv(paste0(rich.input.dir, "/", input.string, "_", names(models)[i], ".csv"))
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
        stop("check model.type")
      }
    }
    ## define plot data frame
    line.df <- get_model_data(models[[i]], type = "pred", terms = xy[1])
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
    scatter
    ## plot
    pdf(file = paste0(output.dir, "/", gsub(" ", "_", gsub(",", "", plot.title)), ".pdf"))
    print(scatter)
    dev.off()
  }
  }
}
