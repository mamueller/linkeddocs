
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="write_Rd_file",
  signature=signature(obj="nonstandardGenericFunction"),
  def=function(obj,fn,exampleDir=NULL,exampleTrunk=NULL){
    srcref <- utils::getSrcref(obj)
    codeText <- as.character(srcref,useSource=T)
    l <- extract.xxx.chunks(codeText)
  	name <-attr(obj,'generic')[[1]]
    pp('name')
    fff <- obj
    fdo=genericFunctionDocObject(name=name,l=l,functionObject=fff)
    write_Rd_file(fdo,fn)
}
)
