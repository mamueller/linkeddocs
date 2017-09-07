
extract.file.parse <- function # File content analysis
### Using the base \code{parse} function, analyse the file to link
### preceding "prefix" comments to each active chunk. Those comments form
### the default description for that chunk. The analysis also looks for
### S4 class "setClass" ,R.oo setConstructorS3  R.methodsS3::setMethodS3
### or S4 setMethod calls in order to link the documentation of those properly.
(code,
### Lines of R source code in a character vector - note that any
### nested \code{source} statements are \emph{ignored} when scanning
### for class definitions.
 env
 ### the environment in which the code has been evaluated before.
 ### This is e.g. iportant to make sure that we can evaluate expressions 
 ### like signature definitions for methods 
 ){
  res <- list()
  old.opt <- options(keep.source=TRUE)
  parsed <- try(parse(text=code))
  options(old.opt)
  if ( inherits(parsed,"try-error") ){
    stop("parse failed with error:\n",parsed)
  }
  chunks <- attr(parsed,"srcref")
  last.end <- 0
  for ( k in 1:length(parsed) ){
    start <- chunks[[k]][1]
    ##details<< If the definition chunk does not contain a
    ## description, any immediately preceding sequence consecutive
    ## "prefix" lines will be used instead.
    default.description <- NULL
    while ( start > last.end+1
           && grepl(prefix,code[start-1],perl=TRUE) ){
      start <- start-1
    }
    if ( start < chunks[[k]][1] ){
      default.description <- decomment(code[start:(chunks[[k]][1]-1)])
    } else {
      default.description <- NA_character_;
    }
    ##details<< Class and method definitions can take several forms,
    ## determined by expression type: \describe{
    ## \item{assignment (<-)}{Ordinary assignment of value/function;}
    ## \item{setClass}{Definition of S4 class;}
    ## \item{setMethod}{Definition of a method of a S4 generic;}
    ## \item{setConstructorS3}{Definition of S3 class using R.oo package;}
    ## \item{R.methodsS3::setMethodS3}{Definition of method for S3 class using R.oo package.}}
    ## Additionally, the value may be a name of a function defined elsewhere,
    ## in which case the documentation should be copied from that other definition.
    ## This is handled using the concept of documentation links.
    lang <- parsed[[k]]
    chars <- as.character(lang)
    expr.type <- chars[1]
    parent <- NA_character_

    if ( expr.type == "<-" || expr.type == "setConstructorS3" ){
      object.name <- chars[2]
      ## If the function definition is not embedded within the call, then
      ## the parent is that function. Test whether the the third value
      ## looks like a name and add it to parents if so.
      if ( grepl("^[\\._\\w]+$",chars[3],perl=TRUE) ){
        parent <- chars[3]
      }
      res[[object.name]] <- new("DocLink",name=object.name,
                                created=expr.type,
                                parent=parent,
                                code=paste(chunks[[k]],sep=""),
                                description=default.description)
#    } else if ( expr.type == "setClass" ){
#      object.name <- chars[2]
#      res[[object.name]] <- new("DocLink",name=object.name,
#                                created=expr.type,
#                                parent=parent,
#                                code=paste(chunks[[k]],sep=""),
#                                description=default.description)
#
    } else if ( expr.type == "R.methodsS3::setMethodS3" || expr.type ==  "R.methodsS3::R.methodsS3::setMethodS3"){
      ##details<< The \code{R.methodsS3::setMethodS3} calls introduce additional
      ## complexity: they will define an additional S3 generic (which
      ## needs documentation to avoid warnings at package build time)
      ## unless one already exists. This also is handled by "linking"
      ## documentation. A previously unseen S3generic is linked to the
      ## first defining instances, subsequent definitions of that S3generic
      ## also link back to the first defining instance.
      S3generic.name <- chars[2]
      object.name <- paste(S3generic.name,chars[3],sep=".")
      if ( is.null(res[[S3generic.name]]) ){
        ## TDH 9 April 2012 Do NOT add \\link in S3generic.desc below,
        ## since it causes problems on R CMD check.
        ##* checking Rd cross-references ... WARNING
        ##Error in find.package(package, lib.loc) : 
        ##  there is no package called ‘MASS’
        ##Calls: <Anonymous> -> lapply -> FUN -> find.package

        S3generic.desc <-
          paste("Generic method behind \\code{",object.name,"}",sep="")
        res[[S3generic.name]] <- new("DocLink",
                                   name=S3generic.name,
                                   created=expr.type,
                                   parent=object.name,
                                   code=NA_character_,
                                   description=S3generic.desc)
      } else {
        parent <- res[[S3generic.name]]@parent
      }
      ## If the function definition is not embedded within the call, then
      ## the parent is that function. Test whether the the fourth value
      ## looks like a name and add it to parents if so.
      if ( grepl("^[\\._\\w]+$",chars[4],perl=TRUE) ){
        parent <- c(chars[4],parent)
      }
      res[[object.name]] <- new("DocLink",name=object.name,
                                created=expr.type,
                                parent=parent,
                                code=paste(chunks[[k]],sep=""),
                                description=default.description)
    } else if (expr.type == "setMethod" ) {
      
      NamedArgs=rewriteSetMethodArgs(lang)
      genName=NamedArgs[["f"]]

      sigexp=NamedArgs[["signature"]]
      gen <- getGeneric(genName,env)
      sig=matchSignature(eval(sigexp,env),gen,env)
      N <- methodDocName(genName,sig)
      object.name <- N

      ## If the function definition is not embedded within the call, then
      ## the parent is that function. Test whether the value for "definition"
      ## looks like a funktion name and add it to parents if so.
      def=paste(as.character(NamedArgs[["definition"]]),collapse="\n")
      if ( grepl("^[\\._\\w]+$",def,perl=TRUE) ){
        parent <- def
      }
      res[[object.name]] <- new("DocLink",name=object.name,
                                created=expr.type,
                                parent=parent,
                                code=paste(chunks[[k]],sep=""),
                                description=default.description)
    }else { 
      ## do nothing maybe a issue a warning later
    }
  }
  invisible(res)
### Returns an invisible list of .DocLink objects.
}
