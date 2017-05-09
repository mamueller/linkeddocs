#
# vim:set ff=unix expandtab ts=2 sw=2:
functionDocObject<-setClass(Class="functionDocObject",contains="docObject")
#-------------------------------------------------------------------------
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

setMethod(
  f="write_Rd_file",
  signature=signature(obj="docObject",fn="character"),
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
