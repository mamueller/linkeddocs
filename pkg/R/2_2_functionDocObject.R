#
# vim:set ff=unix expandtab ts=2 sw=2:
functionDocObject<-setClass(Class="functionDocObject",contains="docObject")
#-------------------------------------------------------------------------
setMethod(
  f="Rd_usage_lines",
  signature=signature(obj="functionDocObject"),
  definition=function(obj){
    arglistStr=arglistStr(get_functionObject(obj))
    usageString<-sprintf("%s(%s)",obj@name,arglistStr)
    return(usageString)
  }
)
#-------------------------------------------------------------------------
setMethod(
  f='get_xxx_chunks',
  signature=signature(obj="functionDocObject"),
  definition=function(obj){
      
      codeText <- get_code(obj)
      
      fobj <- get_functionObject(obj)
      srcRef <- utils::getSrcref(fobj)
      leadingComments<- leadingComments(
        getSrcFilename(fobj,full.names=TRUE),
        pos <- utils::getSrcLocation(srcRef)
      )
      #pp('lc3')
      leadingDesc <- gsub("^[ \t(,#]*", "",leadingComments)
      leadingDesc <- leadingDesc[!grepl('^ *$',leadingDesc)]
      l <- extract.xxx.chunks(codeText)
      pl <- prefixed.lines(codeText)
      pl[['description']] <- append(leadingDesc,pl[['description']])
      #l[['description']] <- append(pl[['description']],l[['description']])
      l <- combine(l,pl)
      tit_list <- title.from.firstline(codeText)
      #fixme mm:
      # at the moment title.from.firstline(codeText) returns a list
      # which is unnecessary, it should be changed to a character vector or NULL
      # as soon as the old version is not needed any more
      if(is.null(tit_list[['title']])){tit_list <- obj@name}
      l[['title']] <- tit_list
      return(l)
  }
)
