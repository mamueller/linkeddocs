#
# vim:set ff=unix expandtab ts=2 sw=2:

# define a list like datatype and constructor that we can define methods for
docObject<-setClass(
  Class="docObject",
  slots=c(
    name="character",
    functionObject="function",
    pkgDir='character'
  )
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_argument_lines",
  signature=signature(obj="docObject"),
  definition=function(obj){
    d=get_xxx_chunks(obj)
    functionObject <- get_functionObject(obj)
    nd=names(d)
    # fixme:mm
    # the list d contains the documentation  of the argument "a" 
    # under the key "item{a}" 
    # this is due to inlinedocs original way of parsing
    # it would however be much more convinient here to use "a" directly
    # we would have to do this at the place where the item{a} is created
    item_names<-nd[grepl('item[{].*[}]',nd)]
    
    # it is possible that some or all arguments where 
    # not documented
    # we can check this by interrogation of the function
    documented_args<-documented_args(obj)
    function_args<-names(formals(functionObject))
    checkWarnArgs(obj@name,function_args,documented_args)
    
    itemString<-"\n"
    for (n in function_args){
      itemString<- ifelse(
        is.element(n,documented_args),
        sprintf("%s\\%s{%s}\n",itemString,wrapArgName(n),d[[wrapArgName(n)]]),
        sprintf("%s\\%s{}\n",itemString,wrapArgName(n))
      )
    }
    return(itemString)
  }
)
#-------------------------------------------------------------------------
setMethod(
  f="documented_args",
  signature=signature(obj="docObject"),
  definition=function(obj){
    d   <-  get_xxx_chunks(obj)
    nd  <-  names(d)
    #fixme mm:
    # see the other fixme
    # I would rather only once (at parse time) 
    # decide by regexpr that something is a function argument
    # and then store the result in a slot of a class instead
    # of wrapping it in a string to find it again later.
    # This is hidden duplication since you have to know  
    # that an argument ends up in an  "item{arg}". 
    # We have to change this in the parser at creation.
    item_names<-nd[grepl('item[{].*[}]',nd)]
    var_names <-unlist(
      lapply(
        X=item_names,
        FUN=function(s){
          v<-unWrapArgName(s)
          return(v)
        }
      )
    )
    return(var_names)
  }
)
#-------------------------------------------------------------------------
example_references <- function(codeText){
    l <- extract.xxx.chunks(codeText)
    key <- 'exampleFunctionsFromFiles'
    if(is.element(key,names(l))){
      refs <- unlist(str_split(l[key],'\n'))
    }else{
      refs <- NULL
    }
    refs
}
#-------------------------------------------------------------------------
external_example_lines <- function(obj){
  res <- NULL
  codeText <- get_code(obj)
  refs <- example_references(codeText)
  if(!is.null(refs)){
    res <- unlist(lapply(refs,example_lines_from_file,obj@pkgDir))
  }
  return(res)
}

#-------------------------------------------------------------------------
setMethod(
  f="Rd_example_lines",
  signature=signature(obj="docObject"),
  definition=function(obj){
    codeText <- get_code(obj)
    l <- extract.xxx.chunks(codeText)
    # first add the examples that are in the comments
    exlines <- l['examples']
    # then look for examples in external files
    ext_exs<- external_example_lines(obj)
    if(!is.null(ext_exs)){
    exlines <- append(exlines,"# examples from external files")
    exlines <- c(exlines,ext_exs) 
    }
    exlines <- unlist(exlines)
    return(exlines)
  }
)

#-------------------------------------------------------------------------
setMethod(
  f="write_Rd_file",
  signature=signature(obj="docObject",fn="character"),
  def=function(
      obj,
      fn
    ){
    d=get_xxx_chunks(obj)
    #the list d is nested e.g. for argumetns
    #we now flatten it so that it only has a 
    # character vector for each section
    flat<-list()
    flat[["name"]]  <- obj@name
    flat[["alias"]] <- obj@name
	  flat[["usage"]] <- Rd_usage_lines(obj)
	  flat[["examples"]] <- Rd_example_lines(obj)

    # for functions it is possible 
    # that no arguments have been documented
    # in setGeneric
    # Of cause the methods will define arguments.
    args<-Rd_argument_lines(obj)
    if (!is.null(args)){flat[["arguments"]]<-args} 
    # add the parts from d that could be extracted 
    #target_secs<-c("title","description","details","references","note","seealso","value","examples")
    target_secs<-setdiff(RdTargetSections(),names(flat))
    for (sec in target_secs){
      if (is.element(sec,names(d))){
        flat[[sec]]<-d[[sec]]
      }
    }
    writeFlattenedListToRd(flat,fn)
  }
)
#-------------------------------------------------------------------------
setMethod(
  f='get_xxx_chunks',
  signature=signature(obj="docObject"),
  definition=function(obj){
  ### fixme:mm
  ### this function should eventually make the @l slot obsolete
  ### which is already done in subclasses that overload the method
  ### At the moment it is just a wrapper for the @l slot access
  ### that makes the overloading possible
  return(obj@l)
  }
)
#-------------------------------------------------------------------------
setMethod(
  ## to be overloaded in subclasses
  f='get_functionObject',
  signature=signature(obj="docObject"),
  definition=function(obj){
  return(obj@functionObject)
  }
)
#-------------------------------------------------------------------------
setMethod(
  f='get_code',
  signature=signature(obj="docObject"),
  definition=function(obj){
      fobj <- get_functionObject(obj)
      srcRef <- utils::getSrcref(fobj)
      codeText <- as.character(srcRef,useSource=T)
    
    # fixme: mm We could already include the leading comments here if we adapted the
    # old extract.xxx.chunks function appropriately
    return(codeText)
    }
)
#-------------------------------------------------------------------------
setMethod(
  f="defaultRdFileName",
  signature=signature(obj="docObject"),
  definition=function(obj){obj@name}
)
