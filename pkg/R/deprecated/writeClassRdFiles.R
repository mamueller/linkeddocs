
# vim:set ff=unix expandtab ts=2 sw=2:
#################################################################
writeClassRdFiles <- function(env,pkgDir,path,exprs,code,desc,nsi){
  #classesFromNamespaceFile<- exportedClasses(pkgDir)
  classesFromNamespaceFile<- nsi$exportedClassNames
  allClassNames<-getClasses(env) 
  
  classDocObjects <- classDocObjects(exprs,code,nsi,env)
  #pp("classDocObjects")
  sapply(
    classDocObjects,
    function(item) {
      filename <- file.path(path,sprintf("%s-class.Rd",item@name))
      write_Rd_file(obj=item,fn=filename)
    }
  )

  #if (inlinedocs.documentNamespaceOnly){
  #  classNames2document<-intersect(classesFromNamespaceFile, allClassNames)
  #}else{
  #  classNames2document<-allClassNames
  #}

  #
  #Docs=list()
  #for ( nn in names(classDocObjects) ){
  #  singleClass.docs <- extract.docs.setClass(classDocObjects[[nn]])
  #  author <- desc[,"Author"]
  #  singleClass.docs[["author"]] <- author
  #  #docname <- paste(nn,"class",sep="-")
  #  docname <- nn
  #  Docs[[docname]] <- singleClass.docs
  #}
  ## create a parallel list of fixed names indexed as the original names
  #fixedClassNames <- fixPackageFileNames(classNames2document)
  #names(fixedClassNames) <- classNames2document


  #sapply(classNames2document, function(item) {
  #  mmPromptClass(
  #    item, 
  #    filename = file.path(path,sprintf("%s-class.Rd.old", fixedClassNames[item])), 
  #    where = env,
  #    clDoc = Docs[[item]],
  #    nsi=nsi
  #  )
  #})
}
