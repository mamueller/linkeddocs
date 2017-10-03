
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="get_docObject",
  signature=signature(obj="nonstandardGenericFunction",pkgDir='character',srcref='missing'),
  def=function(obj,pkgDir){
    fdo=genericFunctionDocObject(
      name=obj@generic[[1]],
      #l=l,
      functionObject=obj
    )
    return(fdo)
    }
 )
