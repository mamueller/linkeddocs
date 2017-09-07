
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="get_docObject",
  signature=signature(obj="classRepresentation",pkgDir='character'),
  def=function(obj,pkgDir){
    clName <-obj@className[[1]]
    cdo <- classDocObject(
      name=clName,
      functionObject=function(){}, ### fixme mm: this slot should be removed from the parent class
      pkgDir=pkgDir,
      clrep=obj 
      )
    }
)
