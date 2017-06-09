
## vim:set ff=unix expandtab ts=2 sw=2:
  findText <- function(obj){
  srcref <- utils::getSrcref(obj)
  #print(srcref)
  codeFileName<-utils::getSrcFilename(obj,full.names=T)
  codeText <- as.character(srcref,useSource=T)
  lines <- readLines(codeFileName)
  pos <- utils::getSrcLocation(obj)
  line <- lines[pos-1]
  while(grepl('^\\s*###',line) && pos >1){
    pos <- pos-1
    line <- lines[pos]
    codeText<- c(line,codeText)
  }
  return(codeText)
}
