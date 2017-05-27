
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="write_Rd_file",
  signature=signature(obj="MethodDefinition"),
  def=function(obj,fn,exampleDir=NULL,exampleTrunk=NULL){
    
    genName <- attr(obj,'generic')
    sig <-attr(obj,'defined')
    
    srcref <- utils::getSrcref(obj)
    codeText <- as.character(srcref,useSource=T)
    pp('codeText')
    expr <- parse(text=codeText)
    nsEnv <- environment(slot(obj,name='.Data'))
    fff <- eval(expr,nsEnv)
    
    
    Text <- findText(obj)
    #pp('Text')
    pe(quote(prefixed.lines(codeText)))
    l <- extract.xxx.chunks(codeText)
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
