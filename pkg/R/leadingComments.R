
## vim:set ff=unix expandtab ts=2 sw=2:
leadingComments <- function(fileName,pos){
  print("#######################433 here I am #############################")
  pp('fileName')
  code <- readLines(fileName)
  print(code)
  leadingComments <- ''
  line <- code[pos-1]
  pos <- pos-1
  while(grepl('^\\s*###',line) && pos >1){
      leadingComments<- c(line,leadingComments)
      pos <- pos-1
      line <- code[pos]
      #codeText<- c(line,codeText)
  }
  return(leadingComments)
}
