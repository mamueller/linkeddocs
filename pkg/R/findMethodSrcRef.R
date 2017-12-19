# vim:set ff=unix expandtab ts=2 sw=2:
findMethodSrcRef<- function(results,genName){
		# we want to find the source reference for every exported method
		# find out the name of the generic and find the code that registered the method
	  ff <- function(entry){
						retval <- FALSE
						res <- entry[['res']]
						if (inherits(res,'classGeneratorFunction')){
							retval <- res@className==clname
						}
						retval
		}
	  bv <- unlist(lapply(results,ff))
		srcRefs <- results[bv]
    if (length(srcRefs)>1){
      print(        
        srcRefs
      )
			stop(
        sprintf(
          'Found more than one definition for Class %s ',
          clname
        )
      )
		}
		sr<- srcRefs[[1]][['srcref']]
    firstLine <- getSrcLocation(sr)
    fileName<- getSrcFilename(sr)
    sr   
}
