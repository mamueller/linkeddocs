
# vim:set ff=unix expandtab ts=2 sw=2:
############################################################
documentableMeths<- function(e,pkgDir){
  ### find out which generics have any documentable methods

  allGens=as.character(getGenerics(where=e))
  
  ## we also want generics not defined by us since we may provide methods for
  ## them
  #allGens=as.character(getGenerics()) 
 

  decide=function(genName){
    GenHasAnyMethodWithSrc(genName,e) 
  }
  GensWithDocMethods=allGens[unlist(sapply(allGens,decide))]
  # now we can make a list of list
  # containing the Methods we want to documents ordered after the name of there Generics
  documentableMeths=list()
  for (genName in GensWithDocMethods){
  	documentableMeths[[genName]]<-MethodsWithSrcRefForGen(genName,e)
  }
  documentableMeths 
}
