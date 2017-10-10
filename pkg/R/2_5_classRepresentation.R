
## vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
setMethod(
  f="get_docObject",
  signature=signature(obj="classRepresentation",pkgDir='character',srcref='srcref'),
  def=function(obj,pkgDir,srcref){
    clName <-obj@className[[1]]
    cdo <- classDocObject(
      name=clName,
      functionObject=function(){}, ### fixme mm: this slot should be removed from the parent class
      pkgDir=pkgDir,
      clrep=obj,
      srcref=srcref
      )
    }
)
#-------------------------------------------------------------------------
setMethod(
  f="findSrcRef",
  signature=signature(obj="classRepresentation",results='list'),
  definition = function(obj,results){
		# we want to find the source reference for every exported class
		# find out the name of the class and find the code that created it
	  # we are looking for an expression that created a classGeneratorFuntion (this is what setClass returns) 
	  # but this classGeneratorFunction might not be bound to an environment # It is only bound if it was assigned by a statement like
	  # M <- setClass("M"...
    clname <- obj@name
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
    sr   
  }
)
