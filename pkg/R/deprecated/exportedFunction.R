
############################################################
exportedFunctions=function
### get the exported functions from the NAMESPACE file
(tD){
  funcNames=exported("export\\(",tD)
  return(funcNames)
}
