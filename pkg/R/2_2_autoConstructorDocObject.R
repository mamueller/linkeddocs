#
# vim:set ff=unix expandtab ts=2 sw=2:
autoConstructorDocObject<-setClass(Class="autoConstructorDocObject",contains="functionDocObject")
#-------------------------------------------------------------------------
#setMethod(
#  f="Rd_usage_lines",
#  signature=signature(obj="autoConstructorDocObject"),
#  definition=function(obj){
#    arglistStr=arglistStr(get_functionObject(obj))
#    usageString<-sprintf("%s(%s)",obj@name,arglistStr)
#    return(usageString)
#  }
#)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_argument_lines",
  signature=signature(obj="autoConstructorDocObject"),
  definition=function(obj){
    functionObject <- get_functionObject(obj)
    function_args<-names(formals(functionObject))
    itemString<-"\n"
    for (n in function_args){
      itemString <- sprintf("%s\\%s{%s}\n",itemString,wrapArgName(n),'Please look at the slots of the class!')
    }
    return(itemString)
    #sprintf('\\item{...}{ Look at the slots of the class for suitable arguments!}') 
  }
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_lines",
  signature=signature(obj="autoConstructorDocObject"),
  def=function(
      obj
    ){
    flat <- list()
    flat[['name']] <- obj@name
    flat[['alias']] <- obj@name
    flat[['title']] <- obj@name
    flat[['usage']] <- Rd_usage_lines(obj)
    flat[['arguments']] <- Rd_argument_lines(obj)
    flat[['description']] <- 'This function was automatically created by \\code{setClass(})'
    return(flat)
  }
)
