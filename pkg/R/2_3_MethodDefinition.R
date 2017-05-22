
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="write_Rd_file",
  signature=signature(obj="MethodDefinition"),
  def=function(obj,fn,exampleDir=NULL,exampleTrunk=NULL){
    
    
    srcref <- utils::getSrcref(obj)

    codeText <- as.character(srcref,useSource=T)
    pp('codeText')
    expr <- parse(text=codeText)
    fff <- eval(expr)
    
    
    Text <- findText(obj)
    #pp('Text')
    pe(quote(prefixed.lines(codeText)))
    l <- extract.xxx.chunks(codeText)
    genName <- 'fake'
    sig <-attr(obj,'defined')
    print(sig)
    N<-methodDocName(genName,sig)
    
    #flat<-list()
	  #flat[["usage"]]  <-Rd_usage_lines(obj)
    #print(flat)
    mdo=methodDocObject(
      l=l,
      name=N,
      genName=genName,
      sig=sig,
      src=codeText,
      functionObject=fff
    ) 
    write_Rd_file(mdo,fn)
    }
)    
