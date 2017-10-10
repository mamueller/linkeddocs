
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="get_docObject",
  signature=signature(obj="nonstandardGenericFunction",pkgDir='character',srcref='srcref'),
  def=function(obj,pkgDir,srcref){
    fdo=genericFunctionDocObject(
      name=obj@generic[[1]],
      pkgDir=pkgDir,
      functionObject=obj,
      srcref=srcref 
    )
    return(fdo)
    }
)
