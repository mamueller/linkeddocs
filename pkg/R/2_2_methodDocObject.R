#
# vim:set ff=unix expandtab ts=2 sw=2:
methodDocObject<-setClass(Class="methodDocObject",contains="docObject",slots=c(methDef='MethodDefinition'))
#-------------------------------------------------------------------------
setMethod(
  f="Rd_usage_lines",
  signature=signature(obj="methodDocObject"),
  definition=function(obj){
    # To construct the special \S4method{} entry
    # we need to prepare two things:
    # 1.) the arguments which we get in the same way as for normal
    #     function
    argStr=arglistStr(get_functionObject(obj))
    
    # 2.) the signature which might be longer than the arglist
    #     since the method might define "missing" args

  	d       <-  get_xxx_chunks(obj)
  	genName <-  obj@methDef@generic
  	sig     <-  obj@methDef@defined
    sigStr<-paste0(as.vector(sig),collapse=",")
    S4String<-paste0("\\S4method{",genName,"}{",sigStr,"}(",argStr,")",sep="")
    return(S4String)
  }
)
#-------------------------------------------------------------------------
setMethod(
  f='get_functionObject',
  signature=signature(obj="methodDocObject"),
  definition=function(obj){
    #md <- obj@methDef
    #srcRef <- utils::getSrcref(md)
    #codeText <- as.character(srcRef,useSource=T)
    codeText <- get_code(obj)
    expr <- parse(text=codeText)
    nsEnv <- environment(slot(obj@methDef,name='.Data'))
    fff <- eval(expr,nsEnv)
    return(fff)
  }
)
#-------------------------------------------------------------------------
setMethod(
  f='get_code',
  signature=signature(obj="methodDocObject"),
  definition=function(obj){
    md <- obj@methDef
    srcRef <- utils::getSrcref(md)
    codeText <- as.character(srcRef,useSource=T)
    
    # fixme: mm We could already include the leading comments here if we adapted the
    # old extract.xxx.chunks function appropriately
    return(codeText)
    }
)
#-------------------------------------------------------------------------
setMethod(
  f='get_xxx_chunks',
  signature=signature(obj="methodDocObject"),
  definition=function(obj){
    codeText <- get_code(obj)

    md <- obj@methDef
    srcRef <- utils::getSrcref(md)
    fn <- getSrcFilename(md,full.names=TRUE)
    pos  <-  utils::getSrcLocation(srcRef)
    
    leadingComments<- leadingComments( fn,pos)
    leadingDesc <- gsub("^[ \t(,#]*", "",leadingComments)
    leadingDesc <- leadingDesc[!grepl('^ *$',leadingDesc)]
    l <- extract.xxx.chunks(codeText)
    pl <- prefixed.lines(codeText)
    pl[['description']] <- append(leadingDesc,pl[['description']])
    l[['description']] <- append(pl[['description']],l[['description']])
    return(l)
  }
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_argument_lines",
  signature=signature(obj="methodDocObject"),
  definition=function(obj){
    d   <-get_xxx_chunks(obj)
  	sig <-obj@methDef@defined
    functionObject <- get_functionObject(obj)
    nd=names(d)
    # fixme:mm
    # the list d contains the documentation  of the argument "a" 
    # under the key "item{a}" 
    # this is due to inlinedocs original way of parsing
    # it would however be much more convinient here to use "a" directly
    # we would have to do this at the place where the item{a} is created
    #item_names<-nd[grepl('item[{].*[}]',nd)]
    
    # it is possible that some or all arguments where 
    # not documented
    # we can check this by interrogation of the function
    documented_args<-documented_args(obj)
    function_args<-names(formals(functionObject))
    checkWarnArgs(obj@name,function_args,documented_args)
    itemString<-"\n"
    #for (n in intersect(function_args,names(sig))){
    for (n in function_args){
      #addition<- ifelse(
      #  is.element(n,documented_args),
      #  sprintf(": of class %s, %s",sig[[n]],d[[wrapArgName(n)]]),
      #  sprintf(": of class %s",sig[[n]])
      #)
      addition<-""
      if ( is.element(n,names(sig)) & is.element(n,documented_args) ){
        addition<- sprintf(": of class %s, %s",sig[[n]],d[[wrapArgName(n)]])
      } else {
        if (is.element(n,documented_args)){ addition<- sprintf(":  %s",d[[wrapArgName(n)]]) }
        if (is.element(n,names(sig))){ addition<- sprintf(": of class %s",sig[[n]]) }
      }
      itemString<-sprintf("%s\\%s{%s}\n",itemString,wrapArgName(n),addition)
    }
    return(itemString)
  }
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_lines",
  signature=signature(obj="methodDocObject"),
  def=function(
      obj
    ){
    d <- get_xxx_chunks(obj)
    #the list d is nested e.g. for argumetns
    #we now flatten it so that it only has a 
    # character vector for each section
    flat<-list()
    flat[["docType"]]<-"methods"
    flat[["name"]]   <-obj@name
    codeText <- get_code(obj)
    #print(codeText)
    title <- title.from.firstlineNew(codeText)
    flat[["title"]]  <- ifelse(
                          test=is.null(title),
                          yes=obj@name,
                          no=sprintf('%s \n %s',obj@name,title)
                        )
    flat[["alias"]]  <-obj@name

    #fixme:
    # workaround for [[<- sice for unexplainable reasons the cran checks complain about the usage
    # lien although the very similar usage line for [[ works
    if (obj@methDef@generic!= "[[<-"){
	     flat[["usage"]]  <-Rd_usage_lines(obj)
    }

	  flat[["arguments"]]=Rd_argument_lines(obj)
	  flat[["examples"]]=Rd_example_lines(obj)
    # add the parts from d that could be extracted 
    #target_secs<-c("description","references","note","value")
    target_secs<-RdTargetSections()
    for (sec in target_secs){
      if (is.element(sec,names(d))){
        flat[[sec]]<-d[[sec]]
      }
    }
    return(flat)
  }
)

#-------------------------------------------------------------------------
setMethod(
  f="defaultRdFileName",
  signature=signature(obj="methodDocObject"),
  def=function(
      obj
     ){
        uniqueMethodFileNameTrunk(obj@methDef) 
  }
)
