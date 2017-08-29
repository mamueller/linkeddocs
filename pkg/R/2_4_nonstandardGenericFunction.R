
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="write_Rd_file",
  signature=signature(obj="nonstandardGenericFunction"),
  def=function(obj,fn){
    srcref <- utils::getSrcref(obj)
    codeText <- as.character(srcref,useSource=T)
    pl <- prefixed.lines(codeText)
    l <- extract.xxx.chunks(codeText)
    for ( n in names(pl)){
      l[[n]] <- append(pl[[n]],l[[n]])
    }
    tit_list <- title.from.firstline(codeText)
  	name <-attr(obj,'generic')[[1]]
    #fixme mm:
    # at the moment title.from.firstline(codeText) returns a list
    # which is unnecessary, it should be changed to a character vector or NULL
    # as soon as the old version is not needed any more
  	if ( is.null(tit_list[['title']]) ){
  	  tit_list <- list(title=paste(name,"generic function"))
  	}
    l[["title"]]<-tit_list
    fff <- obj
    fdo=genericFunctionDocObject(name=name,l=l,functionObject=fff)
    write_Rd_file(fdo,fn)
}
)
