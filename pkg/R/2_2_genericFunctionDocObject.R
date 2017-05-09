#
# vim:set ff=unix expandtab ts=2 sw=2:
genericFunctionDocObject<-setClass(Class="genericFunctionDocObject",contains="docObject")
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
  signature=signature(obj="docObject"),
  definition=function(obj){
    d=obj@l
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
  f="Rd_method_lines",
  signature=signature(obj="genericFunctionDocObject"),
  def=function(
      obj
      )
      {
        return(sprintf("\\code{\\link{%s-methods}}",obj@name))
      }
)
#-------------------------------------------------------------------------
setMethod(
  f="write_Rd_file",
  signature=signature(obj="genericFunctionDocObject",fn="character"),
  def=function(
      obj,
      fn
    ){
    d=obj@l
    #the list d is nested e.g. for argumetns
    #we now flatten it so that it only has a 
    # character vector for each section
    flat<-list()
    flat[["name"]]  <- obj@name
    flat[["alias"]] <- obj@name
	  flat[["usage"]] <- Rd_usage_lines(obj)

    # for generic functions it is possible 
    # that no arguments have been documented
    # in setGeneric
    # Of cause the methods will define arguments.
    args<-Rd_argument_lines(obj)
    if (!is.null(args)){flat[["arguments"]]<-args} 

    #methods<-Rd_method_lines(obj)
    #if (!is.null(methods)){flat[["methods"]]<-methods} 
    # add the parts from d that could be extracted 
    target_secs<-c("title","description","references","note","value")
    for (sec in target_secs){
      if (is.element(sec,names(d))){
        flat[[sec]]<-d[[sec]]
      }
    }
    writeFlattenedListToRd(flat,fn)
  }
)

