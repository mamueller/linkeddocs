
mm.examples.from.testfile=function
### extract examples from external files 
(name,inlinedocs.exampleDir,inlinedocs.exampleTrunk,...){
  tsubdir <-inlinedocs.exampleDir 
  trunk<- inlinedocs.exampleTrunk 
  if (is.null(tsubdir)) {
    return(list())# do nothing 
  }
  p <- paste(trunk,name,".R",sep="")
  allfiles=dir(tsubdir)
  L<- allfiles[grepl(pattern=p,allfiles,fixed=TRUE)]
  path=function(l){file.path(tsubdir,l)}
  paths=lapply(L,path)
#print(lapply(paths,file.exists))

  res=list()
  if(length(L)>0){
    exampleTexts= lapply(paths,readLines)
    combinedText <- unlist(exampleTexts)
    res[["examples"]]=combinedText

  }
  res
}
