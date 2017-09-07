
############################################################
exportedGenerics=function
### get the exported generic functions from the NAMESPACE file
(tD){
  # note that there is only a exportMethods statement available
  funcNames=exported("exportMethods",tD)
  return(funcNames)
}
