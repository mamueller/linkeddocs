#preparation (only make sure that we deal with the newest version
library(devtools)
devtools::uninstall('documenting')
devtools::install('documenting')

#document
# first we show how the documenting package works if one of its functions is called
# this works well 
library(documenting)
res <- main('documented')
print(class(res))
print(length(res))
print(res)

## Now we do the same stuff that docomenting::main does but not inside the package but in a function
# defined here 
# This obviously does not work since in the present environment do_something is overloaded
scriptmain <- function(pkgDir){
	rbefore <- do_something(pgkDir)
	devtools::install(pkgDir)
	descFilePath<- normalizePath(file.path(pkgDir,'DESCRIPTION'))
	print(descFilePath)
	pkgName <- as.character(read.dcf(descFilePath,fields=c('Package')))
	library(pkgName,character.only=TRUE,quietly=TRUE)
	rafter <- do_something(pgkDir)
	rspecific <- documented::do_something(pkgDir)
	
	results <- list(rbefore,rafter,rspecific)
	}
print(scriptmain('documented'))

### even the main function of documenting is no longer available but overloaded by the main of the documented package 
res <- main('documented')
print(res)
### but we can still reach it with the fully qualified name
res <- documenting::main('documented')
print(res)

