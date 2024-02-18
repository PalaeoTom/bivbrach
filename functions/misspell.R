misspell <- function(term){
  term <- gsub('ue','u', term, fixed=TRUE)
  term <- gsub('ae','e', term, fixed=TRUE)
  term <- gsub('ll','l', term, fixed=TRUE)
  term <- gsub('ss','s', term, fixed=TRUE)
  term <- gsub('y', 'i', term, fixed=TRUE)
  term
}
