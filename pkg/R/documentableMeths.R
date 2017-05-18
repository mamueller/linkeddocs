
# vim:set ff=unix expandtab ts=2 sw=2:
############################################################
documentableMeths<- function(exportedGens,pkgDir){
  ### find out which generics have any documentable methods

  documentableMeths=list()
  for (genName in exportedGens){
  	documentableMeths[[genName]]<-MethodsWithSrcRefForGen(genName)
  }
  documentableMeths 
}
