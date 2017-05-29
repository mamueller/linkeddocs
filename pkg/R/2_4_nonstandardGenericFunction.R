
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="write_Rd_file",
  signature=signature(obj="nonstandardGenericFunction"),
  def=function(obj,fn,exampleDir=NULL,exampleTrunk=NULL){
    srcref <- utils::getSrcref(obj)
    codeText <- as.character(srcref,useSource=T)
    pl <- prefixed.lines(codeText)
    l <- extract.xxx.chunks(codeText)
    for ( n in names(pl)){
      l[[n]] <- append(pl[[n]],l[[n]])
    }
    l[['title']]<- title.from.firstline(codeText)
  	name <-attr(obj,'generic')[[1]]
    fff <- obj
    fdo=genericFunctionDocObject(name=name,l=l,functionObject=fff)
    write_Rd_file(fdo,fn)
}
)
