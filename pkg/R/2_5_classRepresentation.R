
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="write_Rd_file",
  signature=signature(obj="classRepresentation"),
  def=function(obj,fn,exampleDir=NULL,exampleTrunk=NULL,code){
    clName <-obj@className[[1]]
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
    pos <- utils::getSrcLocation(srcRef)
    codeText <- as.character(srcRef,useSource=T)
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
    pp('leadingDesc')
    
    l <- extract.xxx.chunks(codeText)
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
    l[["title"]]<-tit_list
    on <- sprintf("%s-class",clName)
    l[['description']] <- desc
    l[["name"]] <-on
    l[["alias"]] <- on
    l[["docType"]] <- "class"
    l[["section{Methods}"]] <- Rd_method_lines(obj)

    cl <- Rd_subclass_lines(obj)
    if (!(is.null(cl))){ 
      l[["section{Subclasses}"]] <- cl
    }
    
    cl <- Rd_constructor_lines(obj)
    if (!is.null(cl)){ 
      l[["section{Constructors}"]] <- cl
    }
    
  	name <-attr(obj,'generic')[[1]]
    writeFlattenedListToRd(l,fn)
}
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_method_lines",
  signature=signature(obj="classRepresentation"),
  def=function(
      obj
      ){
        clName <-attr(obj,'className')[[1]]
	pkgName <- attr(attr(obj,'className'),'package')
  	fqPkgName <- sprintf("package:%s",pkgName)
	pkgEnv <- as.environment(fqPkgName)

        # we only link to methods we can see when the package is loaded with library
        # this might include methods for generics that we didn't define like [, [[ $ 
        # and the like but excludes methods for generics that we did not export
        # which is what we want here
        methnms <- intersect(genWithClass(clName,pkgEnv),getGenerics(where=pkgEnv))
        nmeths=length(methnms)

        if (nmeths > 0) {
          .meths.body <- "  \\describe{"
          for (i in 1L:nmeths) {
              
              .sig <- sigsList(methnms[i], where = pkgEnv)
             # pp(".sig")
              for (j in seq_along(.sig)) {
                  # find signatures containing the class we are documenting
                  msigs=match(.sig[[j]], clName)
                  if (!all(is.na(msigs))) {
                     methn.i <- escape(methnms[i])
                    # the signature list might still contain several ANY 
                    # arguments and 
                    # 
                    #mm: we add a \link to the methods here
                    cur <- paste(.sig[[j]], collapse = ",")
                    target_alias=methodDocName(methn.i,.sig[[j]])
                    .meths.body <- c(.meths.body, paste0("    \\item{", 
                      methn.i, "}{\\code{signature", pastePar(.sig[[j]]), 
                      "}: ... }"," \\code{\\link{",target_alias,"}}"))
                    #.methAliases <- paste0(.methAliases, "\\alias{", 
                    #  methn.i, ",", cur, "-method}\n")
                  }
              }
          }
          .meths.body <- c(.meths.body, "\t }")
        }else{
        .meths.body <- paste("No methods defined with class", 
            clName, "in the signature.")
        }
          
   }
)
#-------------------------------------------------------------------------
Rd_subclass_lines<-function(obj){
  l <- NULL
  #pkg <- attr(obj,"package")
  scs <- obj@subclasses
  if (length(scs)>0){
    l<-'\\describe{'
    for(scn in names(scs)){
      l<- c(l,sprintf('\t\\code{\\link{%s-class}}\\cr',scn))
    }
    l<- c(l, "}")
  }
  return(l)
}
#-------------------------------------------------------------------------
Rd_constructor_lines<-function(obj){
  l <- NULL
  clName <-obj@className[[1]]
  if (obj@virtual){
    constructorName <- sprintf('%sSubClassInstance',clName)
  }else{
    constructorName <- clName
  }
  pp('constructorName')
	fqpkgName <- sprintf('package:%s',obj@package)
  possibleConstructor<- tryCatch(
    getFunction(constructorName,where=as.environment(fqpkgName))
    ,
    error=function(e){e}
  )
  if (! inherits(possibleConstructor,'simpleError')){
    l<- as.character(sprintf('\t\\code{\\link{%s}}\\cr',constructorName))
  }
  #if(clName=='DecompOp'){
  #  pp('l')
  #  pp('constructorName')
  #  pe(quote(class(possibleConstructor)))
  #  pe(quote(as.character(sprintf('\t\\code{\\link{%s}}\\cr',constructorName))))
  #  stop()
  #}
  return(l)
}
