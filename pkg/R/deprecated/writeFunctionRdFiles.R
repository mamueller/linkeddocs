#
# vim:set ff=unix expandtab ts=2 sw=2:
#################################################################
writeFunctionRdFiles <- function(path,docs,nsi){
  ### 
  #objs <- sapply(ls(e),get,e,simplify=FALSE)
  # note that the is.function also matches generic function
  #funs<- objs[unlist(sapply(objs,is.function))]
  funs<- nsi[["non_gens"]]
  #gens <- getGenerics(where=e)
  #if (inlinedocs.documentNamespaceOnly){
   #funNamesToDocument <- setdiff( exportedFunctions(pkgDir), gens )
   funNamesToDocument <- names(nsi[["non_gens"]])
  #}else{
  #  funNamesToDocument<-setdiff(names(funs),gens)
  #}

  # note that ls will not find S4 methods for generic functions 
  # these are treated elsewhere
  list0 <- fixPackageFileNames(funNamesToDocument)
  names(list0) <- funNamesToDocument
  sapply(
    funNamesToDocument, 
    function(item) {
      dl <- docs[[item]]
      pp('dl')
      fn <- file.path(path, paste(list0[[item]],".Rd",sep=""))
      fff<-funs[[item]]
      fdo=functionDocObject(name=item,l=dl,functionObject=fff)
      write_Rd_file(fdo,fn)
    }
  )
  
  
}
