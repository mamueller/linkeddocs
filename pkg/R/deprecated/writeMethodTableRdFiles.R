
# vim:set ff=unix expandtab ts=2 sw=2:
#################################################################
writeMethodTableRdFiles <- function(e,pkgDir,path,nsi){
  
  methsByGenName<-nsi[["documentableMeths"]]
  
  for (genName in names(methsByGenName)){
    l <- mmPromptMethods(
      genName=genName,filename=NA,
      exportedMeths=methsByGenName[[genName]],
      where=e,
      nsi
    )
    f=fixPackageFileNames(paste(genName,"-methods.Rd",sep=""))
    p=file.path(path,f)
    cat(unlist(l), file = p, sep = "\n")
  }
}
