
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="write_Rd_file",
  signature=signature(obj="MethodDefinition"),
  def=function(obj,fn,exampleDir=NULL,exampleTrunk=NULL){
    
    genName <- attr(obj,'generic')
    sig <-attr(obj,'defined')
    
    srcRef <- utils::getSrcref(obj)
    codeText <- as.character(srcRef,useSource=T)
    code <- readLines(getSrcFilename(obj,full.names=TRUE))
    pos <- utils::getSrcLocation(srcRef)
    leadingComments <- ''
    pos <- pos-1
    line <- code[pos]
    while(grepl('^\\s*###',line) && pos >1){
      #codeText<- c(line,codeText)
      leadingComments<- c(line,leadingComments)
      pos <- pos-1
      line <- code[pos]
    }
    leadingDesc <- gsub("^[ \t(,#]*", "",leadingComments)
    leadingDesc <- leadingDesc[!grepl('^ *$',leadingDesc)]
    l <- extract.xxx.chunks(codeText)
    pl <- prefixed.lines(codeText)
    pl[['description']] <- append(leadingDesc,pl[['description']])
    l[['description']] <- append(pl[['description']],l[['description']])
    expr <- parse(text=codeText)
    nsEnv <- environment(slot(obj,name='.Data'))
    fff <- eval(expr,nsEnv)
    
    
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
