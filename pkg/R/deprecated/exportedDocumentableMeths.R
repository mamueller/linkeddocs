
# vim:set ff=unix expandtab ts=2 sw=2:
############################################################
exportedDocumentableMeths<- function(e,pkgDir){
# fixme mm:
# this is an intermediate solution 
# It would be much better to use the same functions 
# to parse the NAMESPACE file that are used when the package is 
# actually loaded with library()
# The logic followed by R is a follows (see writing R extensions)
# An exported generic automatically exports all its methods.
# An exported method automatically exports its generic
# which in turn exports all its other methods
# This behaviour applies also to operators like [,[[,$ and so on.
# If we export a method for methods for all other signatures will# be exported


# scenario1
# example [[ 
# If there is any method defined it is automatically exported since 

  dm <- documentableMeths(e,pkgDir)
  #pp("dm",environment()) 
  decide1=function(genName){
     GenHasAnyExposedMethod(genName,e,pkgDir)
  
  }
  indices=unlist(sapply(names(dm),decide1))
  newGens <- dm[indices]

# scenario2
# example "unExportedGeneric" 
# If there is a method defined for a generic that we defined but not 
# exported it is also not exported

  #decide2 <-  function(MethodDescription){
  #  MethodSignatureHasOnlyExportedClasses(MethodDescription,e,pkgDir)
  #}

  #for (genName in names(newGens)){
  #   allMeths=newGens[[genName]]
  #   newGens[[genName]] <- allMeths[sapply(allMeths,decide2)]
  #}
  newGens

}
