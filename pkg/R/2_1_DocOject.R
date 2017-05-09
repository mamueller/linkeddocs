#
# vim:set ff=unix expandtab ts=2 sw=2:

# define a list like datatype and constructor that we can define methods for
docObject<-setClass(Class="docObject",slots=c(name="character",l="list",functionObject="function"))
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
    d   <-  obj@l
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
