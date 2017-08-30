
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="get_docObject",
  signature=signature(obj="nonstandardGenericFunction",pkgDir='character'),
  def=function(obj,pkgDir){
    fdo=genericFunctionDocObject(
      name=obj@generic[[1]],
      #l=l,
      l=list(),#fixme: mm The field is an empty list because I want to get rid of it 
      functionObject=obj
    )
    return(fdo)
    }
 )
