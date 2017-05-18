main <- function(pkgDir){
	rbefore <- do_something(pgkDir)
	
	require(devtools)
	devtools::install(pkgDir)
	descFilePath<- normalizePath(file.path(pkgDir,'DESCRIPTION'))
      	pkgName <- as.character(read.dcf(descFilePath,fields=c('Package')))
	library(pkgName,character.only=TRUE,quietly=TRUE)
	rafter <- do_something(pgkDir)
	rspecific <- documented::do_something(pkgDir)
	
	results <- list(rbefore,rafter,rspecific)
	
	return(results)
}
