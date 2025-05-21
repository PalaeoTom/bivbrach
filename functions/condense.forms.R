condense.forms <- function(data, columns, formations, keys){
  ## add new columns
  data$formation <- ""
  data$lithology1 <- ""
  data$environment <- ""
  for(i in 1:nrow(data)){
    print(i)
    ## get forms
    forms <- unique(unlist(data[i,columns]))
    ## if all blank, skip
    if(all(forms == "")){
      next
    } else {
      ## Remove blanks if any
      if(any(forms == "")){
        forms <- forms[-which(forms=="")]
      }
      ## if just one, add and move on
      if(length(forms) == 1){
        data[i,"formation"] <- forms
        ## find metadata
        data[i,"lithology1"] <- formations[which(formations$updated_name == forms),"lithology"]
        data[i,"environment"] <- formations[which(formations$updated_name == forms),"environment"]
      } else {
        ## if 2, find lith and env for both, combine with just a comma between
        lith <- c()
        env <- c()
        ## populate each, then remove gaps, then flatten
        for(f in forms){
          lith <- c(lith, formations[which(formations$updated_name == f),"lithology"])
          env <- c(env, formations[which(formations$updated_name == f),"environment"])
        }
        ## Remove blanks
        if(any(lith == "")){
          lith <- lith[-which(lith=="")]
        }
        ## Remove blanks
        if(any(env == "")){
          env <- env[-which(env=="")]
        }
        ## Lith
        ## if length = 0, return blank
        if(length(lith) == 0){
          data[i,"lithology1"] <- ""
        } else {
          ## if length 1, return as is
          if(length(lith) == 1){
            data[i,"lithology1"] <- lith
          } else {
            ## Length is more than 1, disassemble and then recombine
            lith.split <- c()
            for(l in lith){
              lith.split <- c(lith.split, str_split_1(l, fixed(',')))
            }
            lith <- str_flatten_comma(lith.split)
            data[i,"lithology1"] <- lith
          }
        }
        ## Env
        ## if length = 0, return blank
        if(length(env) == 0){
          data[i,"environment"] <- ""
        } else {
          ## if length 1, return as is
          if(length(env) == 1){
            data[i,"environment"] <- env
          } else {
            ## Length is more than 1, disassemble and then recombine
            env.split <- c()
            for(l in env){
              env.split <- c(env.split, str_split_1(l, fixed(',')))
            }
            env <- str_flatten_comma(env.split)
            data[i,"environment"] <- env
          }
        }
        ## flatten formations and add to formation columns
        data[i,"formation"] <- str_flatten_comma(forms)
      }
    }
  }
  ## Once cells are populated, categorise using divDyn categorise function
  data$lithCat <- categorize(data$lithology1, keys$lith)
  data$bathCat <- categorize(data$environment, keys$bath)
  data$reefCat <- categorize(data$environment, keys$reefs)
  data$reefCat[data$lithCat == "siliciclastic" & data$environment == "marine indet."] <- "non-reef"
  ## Delete old formations columns
  data[,columns] <- NULL
  ## Return
  return(data)
}
