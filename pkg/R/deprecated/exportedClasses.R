
############################################################
exportedClasses=function
### get the exported Classes from the NAMESPACE file
(tD){
  classnames=exported("exportClasses",tD)
  return(classnames)
}
