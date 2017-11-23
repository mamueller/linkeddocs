#
# vim:set ff=unix expandtab ts=2 sw=2:
classDocObject<-setClass(Class="classDocObject",contains="docObjectWithSrc",slots=c(clrep='classRepresentation'))
#-------------------------------------------------------------------------
setMethod(
  f="Rd_usage_lines",
  signature=signature(obj="classDocObject"),
  definition=function(obj){
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

        ml <- method_lines(clrep)
        if(length(ml)>0){
          lines <- (c(sprintf('Exported methods directly defined for class %s:\n',clName),ml))
        }else{
          lines <- (c(sprintf('No exported methods directly defined for class %s:\n',clName),ml))
        }
        # add information about methods inherited from superclasses
        slm <- Rd_superclass_method_lines(clrep)
        lines <- c(lines,slm)
        return(lines)
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
    if(
      existsFunction(
        constructorName,
        where=as.environment(fqpkgName)
      )
    ){
      l <- c(as.character(sprintf('\t\\code{\\link{%s}}\\cr',constructorName)))
    }
    scs <- clrep@subclasses
    if (length(scs)>0){
      l <- c(l,' Please also look at constructors of non virtual subclasses: ')
      #fixme (we could test if the function realy exists
      l <- c(l,
              sprintf('%s.',
                paste(
                  lapply(
                    names(scs),
                    function(constructorName){
                      if(
                        existsFunction(
                          constructorName,
                          where=as.environment(fqpkgName)
                        )
                      )
                      {
                         s <- sprintf('\\code{\\link{%s}}',constructorName)
                      }else{
                         s <- ''
                      }
                      return(s)
                    }
                  ),
                  collapse=', '
                )
              )
            )
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
        l<- c(l,sprintf('\t\\code{\\linkS4class{%s}}\\cr',scn))
      }
      l<- c(l, "}")
    }
    return(l)
  }
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_title_lines",
  signature=signature(obj="classDocObject"),
  def=function(
    obj
    ){
    codeText <- get_code(obj)
    tit_list <- title.from.firstline(codeText)
    #fixme mm:
    # at the moment title.from.firstline(codeText) returns a list
    # which is unnecessary, it should be changed to a character vector or NULL
    # as soon as the old version is not needed any more
    if ( is.null(tit_list[['title']]) ){
      tit_list <- list(title=paste(obj@name,"S4 class"))
    }
    tit_list[['title']]
  }
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_lines",
  signature=signature(obj="classDocObject"),
  def=function(obj){
    clName <-obj@clrep@className[[1]]
    on <- sprintf("%s-class",clName)
    l <- list()
    d <- get_xxx_chunks(obj)
    # add the parts from d that could be extracted 
    target_secs<-c("description","references","note","value","details")
    for (sec in target_secs){
      if (is.element(sec,names(d))){
        l[[sec]]<-d[[sec]]
      }
    }
    l[["name"]] <-on
    l[["alias"]] <- on
    l[["docType"]] <- "class"
    l[["section{Methods}"]] <- Rd_method_lines(obj)
    

    l[["title"]]  <-Rd_title_lines(obj)
    
    cl <- Rd_subclass_lines(obj)
    if (!(is.null(cl))){ 
      l[["section{Subclasses}"]] <- cl
    }
    cl <- Rd_superclass_lines(obj)
    if (!(is.null(cl))){ 
      l[["section{Superclasses}"]] <- cl
    }
    
    cl <- Rd_constructor_lines(obj)
    if (!is.null(cl)){ 
      l[["section{Constructors}"]] <- cl
    }
	  cl <- Rd_example_lines(obj)
    l[["examples"]] <- cl 
    return(l)
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
