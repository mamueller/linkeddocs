#
# vim:set ff=unix expandtab ts=2 sw=2:
classDocObject<-setClass(Class="classDocObject",contains="docObject",slots=c(clrep='classRepresentation'))
#-------------------------------------------------------------------------
setMethod(
  f="Rd_usage_lines",
  signature=signature(obj="classDocObject"),
  definition=function(obj){
  }
)
#-------------------------------------------------------------------------
setMethod(
  f='get_xxx_chunks',
  signature=signature(obj="classDocObject"),
  definition=function(obj){
    clrep <- obj@clrep
    pkgDir <- obj@pkgDir
    clName  <- obj@name
    pkgR<-normalizePath(file.path(pkgDir,'R'))
    codeFiles <- list.files(pkgR,full.names=TRUE)
    code<- ''
    for (fn in codeFiles){code <- append(code,readLines(fn))}
    # since  srcref does not work for classdefinitions yet we have to find the appropriate piece of code ourselves
    expressions <-  exprs <- parse(text=code,keep.source=TRUE)
    chunks <- attr(exprs,'srcref')
	  f=function(expr){
	    isTRUE(as.character(expr)[[1]]=='setClass' && expr[['Class']]==clName)
	  }
    indices <- which(sapply(expressions,f))
    if (length(indices)<1){
      stop(sprintf('multiple definition of class %s',ClassName))
    }
    srcRef <- chunks[[indices[[1]]]] # this is of class 'srcref'
    #find first line
    codeText <- as.character(srcRef,useSource=T)
    pp('codeText')
    l <- extract.xxx.chunks(codeText)

    pos <- utils::getSrcLocation(srcRef)
    leadingComments <- ''
    line <- code[pos-1]
    while(grepl('^\\s*###',line) && pos >1){
      leadingComments<- c(line,leadingComments)
      pos <- pos-1
      line <- code[pos]
      #codeText<- c(line,codeText)
    }
    leadingDesc <- gsub("^[ \t(,#]*", "",leadingComments)
    leadingDesc <- leadingDesc[!grepl('^ *$',leadingDesc)]
    
    desc <- append(leadingDesc,l[['description']])
    if ( length(desc) < 1 ){ 
        desc <- 'no Description'
    }
    tit_list <- title.from.firstline(codeText)
    #fixme mm:
    # at the moment title.from.firstline(codeText) returns a list
    # which is unnecessary, it should be changed to a character vector or NULL
    # as soon as the old version is not needed any more
    if ( is.null(tit_list[['title']]) ){
      tit_list <- list(title=paste(clName,"S4 class"))
    }
    l[["title"]]<-tit_list[['title']]
    l[['description']] <- desc
    pp('l')
    return(l)

   # on <- sprintf("%s-class",clName)
   # l[["name"]] <-on
   # l[["alias"]] <- on
   # l[["docType"]] <- "class"
   # l[["section{Methods}"]] <- Rd_method_lines(clrep)

   # cl <- Rd_subclass_lines(clrep)
   # if (!(is.null(cl))){ 
   #   l[["section{Subclasses}"]] <- cl
   # }
   # 
   # cl <- Rd_constructor_lines(clrep)
   # if (!is.null(cl)){ 
   #   l[["section{Constructors found by naming convention}"]] <- cl
   # }
  }
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_method_lines",
  signature=signature(obj="classDocObject"),
  definition=function(obj){
  }
)
