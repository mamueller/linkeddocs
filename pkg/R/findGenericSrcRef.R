# vim:set ff=unix expandtab ts=2 sw=2:
findGenericSrcRef<- function(results,genName){
		# we want to find the source reference for every exported generic
    # unfortunately R's own getSrcref function seases to  work if a 
    # generic defines a valueClass
    # This is the reason why we have to look for the source reference ourselves.
   
	  # M <- setClass("M"...
	  filterfunc <- function(entry){
		  retval <- FALSE
      res <- entry[['res']]
      #pe(attributes(res))
      if ('generic' %in% names(attributes(res))){
        resName<-attr(res,'generic')[[1]]
        pp('resName')
		    #if (class(res)=='nonstandardGenereicFucntion'){	
        #browser()
        if (resName== genName){
          text <- as.character(entry[['srcref']])
          retval <- (
            any(
              grepl(pattern="setGeneric",text) #fixme mm:maybe this is a bit too lax since #setGeneric would also be matched
            )
          )
        }
		  }
		  retval
    }
	  bv <- unlist(lapply(results,filterfunc))
    pp('bv')
    if (!any(bv)){
	    return(NULL)	  
    }
    srcRefs <- results[bv]
    if (length(srcRefs)>1){
						stop(sprintf('Found more than one definition for Generic %s in %s',genName,srcRefs))
		}
		sr<- srcRefs[[1]][['srcref']]
    firstLine <- getSrcLocation(sr)
    fileName<- getSrcFilename(sr)
    sr   
}
