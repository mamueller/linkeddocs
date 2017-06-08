#
# vim:set ff=unix expandtab ts=2 sw=2:
classDocObjects<- function  
	(
		parsed, 
		### as returned from base:::parse
		code,
    nsi,
    ### namespace information to be able to sort out which classes where exported
    env
    ### the environment in which the code has been evaluated
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

    if ( expr.type == "setClass" ){
      object.name <- chars[2]
      # mm:
      if (is.element(object.name,nsi$exportedClassNames)){
        # look for mehtods that use instances of this class

        res[[object.name]] <- new("classDocObject",name=object.name,
                                created=expr.type,
                                parent=parent,
                                code=paste(chunks[[k]],sep=""),
                                description=default.description,
                                nsi=nsi,
                                env=env,
                                l=list())
      }
    }else { 
      ## Not sure what to do with these yet. Need to deal with setAs etc.
    }
  }
  invisible(res)
### Returns an invisible list of classDocObject s.
}
