#
# vim:set ff=unix expandtab ts=2 sw=2:
classDocObject<-setClass(Class="classDocObject",contains="docObject",slots=c(clrep='classRepresentation',srcref="srcref"))
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
	  pkgName <- attr(attr(clrep,'className'),'package')
    pkgDir <- obj@pkgDir
    clName  <- obj@name
    srcref<- obj@srcref

    #try to get the srcref info from the src_env

    pkgR<-normalizePath(file.path(pkgDir,'R'))
    codeText <- get_code(obj)
    l <- extract.xxx.chunks(codeText)

    pos <- getSrcLocation(srcref)
    fn <- file.path(pkgR,getSrcFilename(srcref))

    leadingDesc <- gsub("^[ \t(,#]*", "",leadingComments(fn,pos))
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
  f="Rd_superclass_lines",
  signature=signature(obj="classDocObject"),
  def=function(obj){
    clrep <- obj@clrep
	  pkgName <- attr(attr(clrep,'className'),'package')
    fqPkgName <- sprintf("package:%s",pkgName)
    pkgEnv <- as.environment(fqPkgName) 
    exportedClassNames<-getClasses(pkgEnv)
    clnames<- getAllSuperClasses(clrep)
    return(
      unlist(
        lapply(
          intersect(clnames,exportedClassNames),
          function(clname){
            sprintf('\\code{\\link{%s-class}}\\cr',clname)
          }
        )
      )
    )
  }
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_constructor_lines",
  signature=signature(obj="classDocObject"),
  def=function(obj){

  clrep <- obj@clrep 
  l <- NULL
  clName <-clrep@className[[1]]
	fqpkgName <- sprintf('package:%s',clrep@package)
  if (clrep@virtual){
    l <- c('The class is abstract ( \\code{contains "VIRTUAL"}).
           It can therefore not be instanciated directly.
           Look at non virtual subclasses and their constructors!\n') 
    # this is a convention 
    constructorName <- sprintf('General%s',clName)
    possibleConstructor<- tryCatch(
      getFunction(constructorName,where=as.environment(fqpkgName))
      ,
      error=function(e){e}
    )
    if (! inherits(possibleConstructor,'simpleError')){
      l <- c(l, 'There is also an \\href{https://en.wikipedia.org/wiki/Abstract_factory_pattern}{abstract factory} that produces instances of different subclasses depending on the input:\n')
      l<-c(l, as.character(sprintf('\t\\code{\\link{%s}}\\cr',constructorName)))
  }
  }else{
    # this is also a convention: look for a constructor named as the class
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
    clName <-obj@clrep@className[[1]]
    on <- sprintf("%s-class",clName)
    l <- list()
    d <- get_xxx_chunks(obj)
    # add the parts from d that could be extracted 
    target_secs<-c("description","references","note","value")
    for (sec in target_secs){
      if (is.element(sec,names(d))){
        l[[sec]]<-d[[sec]]
      }
    }
    l[["name"]] <-on
    l[["alias"]] <- on
    l[["docType"]] <- "class"
    l[["section{Methods}"]] <- Rd_method_lines(obj)
    l[["title"]]  <-obj@name
    
    cl <- Rd_subclass_lines(obj)
    if (!(is.null(cl))){ 
      l[["section{Subclasses}"]] <- cl
    }
    cl <- Rd_superclass_lines(obj)
    if (!(is.null(cl))){ 
      l[["section{Exported superclasses}"]] <- cl
    }
    
    cl <- Rd_constructor_lines(obj)
    if (!is.null(cl)){ 
      l[["section{Constructors}"]] <- cl
    }
	  cl <- Rd_example_lines(obj)
    l[["section{Examples}"]] <- cl 
    writeFlattenedListToRd(l,fn)
  }
)
#-------------------------------------------------------------------------
setMethod(
  f="defaultRdFileName",
  signature=signature(obj="classDocObject"),
  def=function(obj){
      sprintf("%s-class",obj@name)
  }
)
#-------------------------------------------------------------------------
setMethod(
  f='get_code',
  signature=signature(obj="classDocObject"),
  definition=function(obj){
      codeText <- as.character(obj@srcref,useSource=T)
    
    # fixme: mm We could already include the leading comments here if we adapted the
    # old extract.xxx.chunks function appropriately
    return(codeText)
    }
)
