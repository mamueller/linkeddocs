
#################################################################
methodDocLinks<-function
  (parsed,
  ### as returned from base:::parse
  code,
  env
)
{
  res <- list()
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
    ## This is handled using the concept of documentation links.
    lang <- parsed[[k]]
    chars <- as.character(lang)
    expr.type <- chars[1]
    parent <- NA_character_

    if (expr.type == "setMethod" ) {
      
      NamedArgs=rewriteSetMethodArgs(lang)
      genName=NamedArgs[["f"]]
      sigexp=NamedArgs[["signature"]]
      gen <- getGeneric(genName,env)
      sig=matchSignature(eval(sigexp,env),gen,env)
      #sig=eval(sigexp,env)
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
      ## do not deal with other stuff here but elsewhere
    }
  }
  invisible(res)
### Returns an invisible list of .DocLink objects.
}
