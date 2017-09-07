
############################################################
getMethodSrc <- function(doc.link,e){
  chunk.source <- doc.link@code
  method.name<- doc.link@name
  old.opt <- options(keep.source=TRUE)
  parsed <- try(parse(text=chunk.source))
  options(old.opt)
  if ( inherits(parsed,"try-error") ){
    stop("parse failed with error:\n",parsed)
  }
  lp <- length(parsed) 
  if(lp!=1){
    stop("extract.docs.setMethod:the expected code should be a lingle setMethod expression")
  }


  NamedArgs=rewriteSetMethodArgs(parsed[[1]])
  s <- NamedArgs[["signature"]]
  methodDef=getMethod(
      f=NamedArgs[["f"]],
      signature=eval(NamedArgs[["signature"]]),
      where=e
    )
  src=as.character(getSrcref(unRematchDefinition(methodDef)))
  src
}
