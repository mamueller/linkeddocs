
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="get_docObject",
  signature=signature(obj="MethodDefinition",pkgDir='character'),
  def=function(obj,pkgDir){
    
    genName <- obj@generic
    sig <-obj@defined
    
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
      #l=l,
      l=list(),#fixme: mm The field is an empty list because I want to get rid of it 
      #but cant until it is obsolete in the parent class
      name=N,
      src=codeText,
      methDef=obj,
      functionObject=fff,
      pkgDir=pkgDir
    ) 
    return(mdo)
    }
)    
#-------------------------------------------------------------------------
# possible fixme mm:
# We would  not need this method here if we did not call it directly elsewhere for MethodDefinitions
# In the future, code could actually always create the doc objects and then call the write_Rd_file method of them.
# This is just a convienient delegation
setMethod(
  f="write_Rd_file",
  signature=signature(obj="MethodDefinition"),
  def=function(obj,fn,pkgDir){
    mdo <- get_docObject(obj,pkgDir)
    write_Rd_file(mdo,fn)
    }
)    
