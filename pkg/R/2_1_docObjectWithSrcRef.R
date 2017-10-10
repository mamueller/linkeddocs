#
# vim:set ff=unix expandtab ts=2 sw=2:
# define a class that can handle srcrefs to inherit from it, since some of the 
# docObjects can not rely on built in R fucntions to find their src code while others
# can use 
docObject<-setClass(
  Class="docObjectWithSrc",
  contains=c('docObject','VIRTUAL'),
  slots=c( srcref="srcref")
)
#-------------------------------------------------------------------------
setMethod(
  f='get_code',
  signature=signature(obj="docObjectWithSrc"),
  definition=function(obj){
      codeText <- as.character(obj@srcref,useSource=T)
    
    # fixme: mm We could already include the leading comments here if we adapted the
    # old extract.xxx.chunks function appropriately
    return(codeText)
    }
)
#-------------------------------------------------------------------------
setMethod(
  f='get_xxx_chunks',
  signature=signature(obj="docObjectWithSrc"),
  definition=function(obj){
    pkgDir <- obj@pkgDir
    clName  <- obj@name
    srcref<- obj@srcref

    #try to get the srcref info from the src_env

    codeText <- get_code(obj)
    l <- extract.xxx.chunks(codeText)

    pos <- getSrcLocation(srcref)
    fn <- file.path(normalizePath(file.path(pkgDir,'R')),getSrcFilename(srcref))

    leadingDesc <- gsub("^[ \t(,#]*", "",leadingComments(fn,pos))
    leadingDesc <- leadingDesc[!grepl('^ *$',leadingDesc)]
    
    
    desc <- append(leadingDesc,l[['description']])
    if ( length(desc) < 1 ){ 
        desc <- 'no Description'
    }
    l[['description']] <- desc
    return(l)

  }
)
