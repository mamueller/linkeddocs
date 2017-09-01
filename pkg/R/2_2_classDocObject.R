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
    l <- extract.xxx.chunks(codeText)

    pos <- utils::getSrcLocation(srcRef)
    # fixme mm the following lines could be avoided if we
    # could use the leadingComments function
    # We would have to source the srccode files separatly

    leadingComments <- ''
    pos <- pos-1
    line <- code[pos]
    while(grepl('^\\s*###',line) && pos >1){
      pos <- pos-1
      leadingComments<- c(line,leadingComments)
      line <- code[pos]
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
  def=function(
      obj
      ){
        clrep <- obj@clrep 
        clName <-attr(clrep,'className')[[1]]
	      pkgName <- attr(attr(clrep,'className'),'package')
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
setMethod(
  f="Rd_constructor_lines",
  signature=signature(obj="classDocObject"),
  def=function(obj){

  clrep <- obj@clrep 
  l <- NULL
    l <- 'blub'
  clName <-clrep@className[[1]]
	fqpkgName <- sprintf('package:%s',clrep@package)
  if (clrep@virtual){
    # this is a convention 
    constructorName <- sprintf('%sSubClassInstance',clName)
    possibleConstructor<- tryCatch(
      getFunction(constructorName,where=as.environment(fqpkgName))
      ,
      error=function(e){e}
    )
    if (! inherits(possibleConstructor,'simpleError')){
      l <- c('Since the class is virtual it can not be instanciated directly, but a function:')
      l<-c(l, as.character(sprintf('\t\\code{\\link{%s}}\\cr',constructorName)))
      l <- c(l,'has been found, that produces instances of subclasses.
             Please also look at constructors of non virtual subclasses ')
    }
  }else{
    constructorName <- clName
    possibleConstructor<- tryCatch(
      getFunction(constructorName,where=as.environment(fqpkgName))
      ,
      error=function(e){e}
    )
    if (! inherits(possibleConstructor,'simpleError')){
      l <- c(as.character(sprintf('\t\\code{\\link{%s}}\\cr',constructorName)))
      l <- c(l,' Please also look at constructors of non virtual subclasses ')
    }
  }
  return(l)
  }
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_subclass_lines",
  signature=signature(obj="classDocObject"),
  def=function(obj){
    clrep <- obj@clrep 
    l <- NULL
    #pkg <- attr(clrep,"package")
    scs <- clrep@subclasses
    if (length(scs)>0){
      l<-'\\describe{'
      for(scn in names(scs)){
        l<- c(l,sprintf('\t\\code{\\link{%s-class}}\\cr',scn))
      }
      l<- c(l, "}")
    }
    return(l)
  }
)
#-------------------------------------------------------------------------
setMethod(
  f="write_Rd_file",
  signature=signature(obj="classDocObject",fn='character'),
  def=function(obj,fn){
  }
)
