#!/usr/bin/Rscript
require('devtools')
require('methods')
require('linkeddocs')
pkgName <- 'PrivateAndPublic'
pkgDir <- sprintf('IoTestResults_tmp/PackageTests_3.test.%s/pkg',pkgName)
devtools::install(pkgDir)


taskFunc <- function(pkgEnv,nsEnv,results){
	exClNs <- getClasses(pkgEnv)
	allClNs <- getClasses(nsEnv)
	pe(quote(exClNs))
	pe(quote(allClNs))
	for (clname in exClNs){
		# we want to find the source reference for every exported class
		clRep <- getClass(clname,pkgEnv)
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
		pe(quote(srcRefs[[1]][['srcRef']]))
	}
	
}
results <- compareNsAndPkgEnvs(pkgDir,taskFunc)
print(results)
#linkeddocs::package.skeleton.dx_3(pkgDir)
#dnse <-  devtools::load_all(pkgDir)
##env=dnse$env
#results
#
#denv<- devtools:::create_ns_env(pkgDir)
#print(getClasses(where=denv))
#unload(pkgDir)
#require(pkgName,character.only=TRUE)
#fqPkgName <- sprintf("package:%s",pkgName)
#pkgEnv <- as.environment(fqPkgName) 
#print(getClasses(pkgEnv))
##pkgNsEnv <- asNamespace(pkgName) #
##ECN<-getClasses(pkgEnv)
##acn<-getClasses(pkgNsEnv)
##print(acn)
##detach(fqPkgName,character.only=TRUE,unload=TRUE)
#
