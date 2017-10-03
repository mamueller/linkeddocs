# vim:set ff=unix expandtab ts=2 sw=2:
findClassSrcRef<- function(results,clname){
		# we want to find the source reference for every exported class
		# find out the name of the class and find the code that created it
	  # we are looking for an expression that created a classGeneratorFuntion (this is what setClass returns) 
	  # but this classGeneratorFunction might not be bound to an environment # It is only bound if it was assigned by a statement like
	  # M <- setClass("M"...
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
						stop(fprintf('Found more than one definition for Class %s in %s',clname,srcRefs))
		}
		sr<- srcRefs[[1]][['srcref']]
    firstLine <- getSrcLocation(sr)
    fileName<- getSrcFilename(sr)
    pp('firstLine')
    pp('fileName')
    sr   
}
