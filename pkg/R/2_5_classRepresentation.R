
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
						stop(sprintf('Found more than one definition for Class %s in %s',clname,srcRefs))
		}
		sr<- srcRefs[[1]][['srcref']]
    firstLine <- getSrcLocation(sr)
    fileName<- getSrcFilename(sr)
    sr   
  }
)
#-------------------------------------------------------------------------
setMethod(
  f="Rd_superclass_method_lines",
  signature=signature(obj="classRepresentation"),
  def=function(
    obj
    ){
      clrep <- obj
      clName <-attr(clrep,'className')[[1]]
	    pkgName <- attr(attr(clrep,'className'),'package')
  	  fqPkgName <- sprintf("package:%s",pkgName)
	    pkgEnv <- as.environment(fqPkgName)
      exportedClassNames<-getClasses(pkgEnv)
      clNames<- getAllSuperClasses(clrep)
       
      methnms <- intersect(
        unlist(
          lapply(
            intersect(clNames,exportedClassNames),
            function(clName){
              genWithClass(clName,pkgEnv)
            }
          )
        ),
        getGenerics(where=pkgEnv)
      )
      if(length(methnms)==0){
        return(character(0))
      }else{
        lines <- c(
          '\nMethods inherited from superclasses:\n',
          as.character(
            unlist(
              lapply(
                clNames,
                function(clName){
                  ml <- method_lines(getClass(clName,where=pkgEnv))
                  if(length(ml)>0){
                    return(c(sprintf('from class %s:\n',clName),ml))
                  }else{
                    return(character(0))
                  }
                }
              )
            )
          )
        )
      }
    }
)

#-------------------------------------------------------------------------
setMethod(
  f="method_lines",
  signature=signature(obj="classRepresentation"),
  def=function(clrep){
      clName <-attr(clrep,'className')[[1]]
	    pkgName <- attr(attr(clrep,'className'),'package')
  	  fqPkgName <- sprintf("package:%s",pkgName)
	    pkgEnv <- as.environment(fqPkgName)
      exportedClassNames<-getClasses(pkgEnv)
      clNames<- getAllSuperClasses(clrep)
      methnms <- intersect(genWithClass(clName,pkgEnv),getGenerics(where=pkgEnv))
      nmeths=length(methnms)

      if (nmeths > 0) {
        .meths.body <- "  \\describe{"
        for (i in 1L:nmeths) {
            
            .sig <- sigsList(methnms[i], where = pkgEnv)
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
        .meths.body <- character(0)
      }
      return(.meths.body)
  }
)
