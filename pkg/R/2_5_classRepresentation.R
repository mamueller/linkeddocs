
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="get_docObject",
  signature=signature(obj="classRepresentation",pkgDir='character',srcref='srcref'),
  def=function(obj,pkgDir,srcref){
    clName <-obj@className[[1]]
    cdo <- classDocObject(
      name=clName,
      functionObject=function(){}, ### fixme mm: this slot should be removed from the parent class
      pkgDir=pkgDir,
      clrep=obj,
      srcref=srcref
      )
    }
)
