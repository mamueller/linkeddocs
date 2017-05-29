
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="write_Rd_file",
  signature=signature(obj="classRepresentation"),
  def=function(obj,fn,exampleDir=NULL,exampleTrunk=NULL,code){
    clName <-attr(obj,'className')[[1]]
    # since  srcref does not work for classdefinitions yet we have to find the appropriate piece of code ourselves
    expressions <-  exprs <- parse(text=code,keep.source=TRUE)
    chunks <- attr(exprs,'srcref')
	  f=function(expr){
	    isTRUE(as.character(expr)[[1]]=='setClass' && expr[['Class']]==clName)
	  }
    indices <- which(sapply(expressions,f))
    if (length(indices)<1){
      stop(sprintf('multiple definition of class %s',ClassName))
    }
    srcRef <- chunks[[indices[[1]]]] # this is of class 'srcref'
    #find first line
    pos <- utils::getSrcLocation(srcRef)
    codeText <- as.character(srcRef,useSource=T)
    leadingComments <- ''
    line <- code[pos-1]
    while(grepl('^\\s*###',line) && pos >1){
      pos <- pos-1
      line <- code[pos]
      #codeText<- c(line,codeText)
      leadingComments<- c(line,leadingComments)
    }
    leadingDesc <- gsub("^[ \t(,#]*", "",leadingComments)
    leadingDesc <- leadingDesc[!grepl('^ *$',leadingDesc)]
    
    l <- extract.xxx.chunks(codeText)
    l[['description']] <- append(leadingDesc,l[['description']])
    l[['title']]<- gsub('^.*#','',codeText[[1]])
    on <- paste(clName,"class",sep="-")
    l[["name"]] <-on
    l[["alias"]] <- on
    l[["docType"]] <- "class"
    l[["section{Methods}"]] <- Rd_method_lines(obj)
    pp('l')
  	name <-attr(obj,'generic')[[1]]
    writeFlattenedListToRd(l,fn)
}
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_method_lines",
  signature=signature(obj="classRepresentation"),
  def=function(
      obj
      ){
        clName <-attr(obj,'className')[[1]]
	pkgName <- attr(attr(obj,'className'),'package')
  	fqPkgName <- sprintf("package:%s",pkgName)
	pkgEnv <- as.environment(fqPkgName)

        # we only link to methods we can see when the package is loaded with library
        # this might include methods for generics that we didn't define like [, [[ $ 
        # and the like but excludes methods for generics that we did not export
        # which is what we want here
        methnms <- intersect(genWithClass(clName,pkgEnv),getGenerics(where=pkgEnv))
        nmeths=length(methnms)

        if (nmeths > 0) {
          .meths.body <- "  \\describe{"
          for (i in 1L:nmeths) {
              
              .sig <- sigsList(methnms[i], where = pkgEnv)
             # pp(".sig")
              for (j in seq_along(.sig)) {
                  # find signatures containing the class we are documenting
                  msigs=match(.sig[[j]], clName)
                  if (!all(is.na(msigs))) {
                     methn.i <- escape(methnms[i])
                    # the signature list might still contain several ANY 
                    # arguments and 
                    # 
                    #mm: we add a \link to the methods here
                    cur <- paste(.sig[[j]], collapse = ",")
                    target_alias=methodDocName(methn.i,.sig[[j]])
                    .meths.body <- c(.meths.body, paste0("    \\item{", 
                      methn.i, "}{\\code{signature", pastePar(.sig[[j]]), 
                      "}: ... }"," \\code{\\link{",target_alias,"}}"))
                    #.methAliases <- paste0(.methAliases, "\\alias{", 
                    #  methn.i, ",", cur, "-method}\n")
                  }
              }
          }
          .meths.body <- c(.meths.body, "\t }")
        }else{
        .meths.body <- paste("No methods defined with class", 
            obj@name, "in the signature.")
        }
          
   }
)
