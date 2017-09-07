
inherit.docs <- function(
### recursively add documentation inherited from doc.link parents 
  parsed, ##<< a list of doc.link objects
  res,    ##<< the list of documentation to be extended
  childName      ##<< the name of the object who possibly inherits
  ){
  in.res <- res[[childName]] #start with the present 
  childsDocLink <-parsed[[childName]] 
  if ( !is.null(childsDocLink) ){
    for ( parent in childsDocLink@parent ){
      if ( !is.na(parent) ){
        if ( is.null(in.res) ){
          in.res <- res[[parent]]
        } else if ( parent %in% names(res) ){
          parent.docs <- res[[parent]]
          for ( nn in names(parent.docs) ){
            if ( !nn %in% names(in.res) ){
              in.res[[nn]] <- parent.docs[[nn]]
            }
          }
      }
    }
    }
  }
  invisible(in.res)
  ### the possibly extended list of documentation
}
