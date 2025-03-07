tidy.formations <- function(forms){
  ## Get all formations with entries
  f <- which(!forms == "")
  ## Add ' formation' to end of each formation
  nonF <- which(str_detect(forms, regex(" series$", ignore_case = T)))
  nonF <- c(nonF, which(str_detect(forms, regex(" group$", ignore_case = T))))
  nonF <- c(nonF, which(str_detect(forms, regex(" subgroup$", ignore_case = T))))
  nonF <- c(nonF, which(str_detect(forms, regex(" supergroup$", ignore_case = T))))
  nonF <- c(nonF, which(str_detect(forms, regex(" clay$", ignore_case = T))))
  nonF <- c(nonF, which(str_detect(forms, regex(" limestone$", ignore_case = T))))
  nonF <- c(nonF, which(str_detect(forms, regex(" dolomite$", ignore_case = T))))
  nonF <- c(nonF, which(str_detect(forms, regex(" chalk$", ignore_case = T))))
  nonF <- c(nonF, which(str_detect(forms, regex(" marble$", ignore_case = T))))
  nonF <- c(nonF, which(str_detect(forms, regex("kalk$", ignore_case = T))))
  nonF <- c(nonF, which(str_detect(forms, regex(" shale$", ignore_case = T))))
  nonF <- c(nonF, which(str_detect(forms, regex(" mudstone$", ignore_case = T))))
  nonF <- unique(nonF)
  forms[setdiff(f, nonF)] <- paste0(forms[setdiff(f, nonF)], " formation")
  ## get rid of capitalisation of nonFs
  forms[which(str_detect(forms, " Series$"))] <- str_replace(forms[which(str_detect(forms, " Series$"))], " Series$", " series")
  forms[which(str_detect(forms, " Group$"))] <- str_replace(forms[which(str_detect(forms, " Group$"))], " Group$", " group")
  forms[which(str_detect(forms, " Subgroup$"))] <- str_replace(forms[which(str_detect(forms, " Subgroup$"))], " Subgroup$", " subgroup")
  forms[which(str_detect(forms, " Supergroup$"))] <- str_replace(forms[which(str_detect(forms, " Supergroup$"))], " Supergroup$", " supergroup")
  forms[which(str_detect(forms, " Clay$"))] <- str_replace(forms[which(str_detect(forms, " Clay$"))], " Clay$", " clay")
  forms[which(str_detect(forms, " Limestone$"))] <- str_replace(forms[which(str_detect(forms, " Limestone$"))], " Limestone$", " limestone")
  forms[which(str_detect(forms, " Dolomite$"))] <- str_replace(forms[which(str_detect(forms, " Dolomite$"))], " Dolomite$", " dolomite")
  forms[which(str_detect(forms, " Chalk$"))] <- str_replace(forms[which(str_detect(forms, " Chalk$"))], " Chalk$", " chalk")
  forms[which(str_detect(forms, " Marble$"))] <- str_replace(forms[which(str_detect(forms, " Marble$"))], " Marble$", " marble")
  forms[which(str_detect(forms, " Shale$"))] <- str_replace(forms[which(str_detect(forms, " Shale$"))], " Shale$", " shale")
  forms[which(str_detect(forms, " Mudstone$"))] <- str_replace(forms[which(str_detect(forms, " Mudstone$"))], " Mudstone$", " mudstone")
  ## return forms
  return(forms)
}
