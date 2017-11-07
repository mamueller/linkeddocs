#
# vim:set ff=unix expandtab ts=2 sw=2:
genericFunctionDocObject<-setClass(Class="genericFunctionDocObject",contains="docObjectWithSrc")
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
setMethod(
  f="Rd_usage_lines",
  signature=signature(obj="genericFunctionDocObject"),
  definition=function(obj){
    arglistStr=arglistStr(obj@functionObject)
    usageString<-sprintf("%s(%s)",obj@name,arglistStr)
    return(usageString)
  }
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_argument_lines",
  signature=signature(obj="genericFunctionDocObject"),
  definition=function(obj){
    d=get_xxx_chunks(obj)
    functionObject=obj@functionObject
    nd=names(d)
    # fixme:mm
    # the list d contains the documentation  of the argument "a" 
    # under the key "item{a}" 
    # this is due to inlinedocs original way of parsing.
    # It would however be much more convinient here to use "a" directly
    # we would have to do this at the place where the item{a} is created
    item_names<-nd[grepl('item[{].*[}]',nd)]
    
    # it is possible that some or all arguments where 
    # not documented
    # we can check this by interrogation of the function
    documented_args<-documented_args(obj)
    function_args<-names(formals(functionObject))
    undocumented_args<-setdiff(function_args,documented_args)
    checkWarnArgs(obj@name,function_args,documented_args)

    itemString<-"\n"
    for (n in function_args){
      cmnt<-ifelse(
        is.element(n,documented_args),
        d[[wrapArgName(n)]],
        "see the method arguments for details"
      )
      itemString<- sprintf("%s\\%s{%s}\n",itemString,wrapArgName(n),cmnt)
    }
    return(itemString)
  }
)

#-------------------------------------------------------------------------
setMethod(
  f="Rd_title_lines",
  signature=signature(obj="genericFunctionDocObject"),
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
      tit_list <- list(title=paste(obj@name,"S4 generic"))
    }
    tit_list[['title']]
  }
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_method_lines",
  signature=signature(obj="genericFunctionDocObject"),
  def=function(
      obj
      )
      {
        functionObject=obj@functionObject
        genName <-functionObject@generic
        pkgName<-functionObject@package
        l <- NULL

        fqPkgName <- sprintf("package:%s",pkgName)
        pkgEnv <- as.environment(fqPkgName) 
		    meths<- findMethods(genName,where=pkgEnv)
        for (m in meths){
          sig <-m@defined
          N<-methodDocName(genName,sig)
          l<- c(l,sprintf('\t\\code{\\link{%s}}\\cr',N))
        }
        return(l)
      }
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_lines",
  signature=signature(obj="genericFunctionDocObject"),
  def=function(
      obj
    ){
    l<-list()
    d <- get_xxx_chunks(obj)
    # add the parts from d that could be extracted 
    target_secs<-c("description","references","note","value")
    for (sec in target_secs){
      if (is.element(sec,names(d))){
        l[[sec]]<-d[[sec]]
      }
    }
    l[["name"]]  <- obj@name
    l[["alias"]] <- obj@name
	  l[["usage"]] <- Rd_usage_lines(obj)
    l[["title"]] <- Rd_title_lines(obj)
    l[["examples"]] <- Rd_example_lines(obj)

    # for generic functions it is possible 
    # that no arguments have been documented
    # in setGeneric
    # Of cause the methods will define arguments.
    args<-Rd_argument_lines(obj)
    if (!is.null(args)){l[["arguments"]]<-args} 

    meths<-Rd_method_lines(obj)
    if (!is.null(meths)){l[["section{Methods}"]]<-meths} 
    # add the parts from d that could be extracted 
    return(l)
  }
)

