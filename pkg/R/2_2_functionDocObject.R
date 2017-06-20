#
# vim:set ff=unix expandtab ts=2 sw=2:
functionDocObject<-setClass(Class="functionDocObject",contains="docObject")
#-------------------------------------------------------------------------
setMethod(
  f="Rd_usage_lines",
  signature=signature(obj="functionDocObject"),
  definition=function(obj){
    arglistStr=arglistStr(obj@functionObject)
    usageString<-sprintf("%s(%s)",obj@name,arglistStr)
    return(usageString)
  }
)

