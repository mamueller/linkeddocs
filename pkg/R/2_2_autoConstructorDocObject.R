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
  f="write_Rd_file",
  signature=signature(obj="autoConstructorDocObject",fn="character"),
  def=function(
      obj,
      fn
    ){
    l <- list()
    l[['name']] <- obj@name
    l[['alias']] <- obj@name
    l[['title']] <- obj@name
    l[['usage']] <- Rd_usage_lines(obj)
    l[['arguments']] <- Rd_argument_lines(obj)
    l[['description']] <- 'This function was automatically created by \\code{setClass(})'
    writeFlattenedListToRd(l,fn)
  }
)
