
# vim:set ff=unix expandtab ts=2 sw=2:
withPackagesLoaded <- function(PkgNames,expr){
  osp <- search()
	fqN <- function(pkgName){return(sprintf('package:%s',pkgName))}
  successfully_loaded <- unlist(lapply(
    PkgNames,
    function(pkgName){
        require(pkgName,character.only=TRUE)
    }
  ))
	print(search())
	myEnv <- new.env()
	res <- eval(expr,envir=myEnv)
  on.exit(
    lapply(rev(PkgNames[successfully_loaded]),
      function(N){
        detach(fqN(N),unload=TRUE,character.only=TRUE)
      }
    )
  )
  return(res)
}






expr <-   quote(
    {
      Model
      Model_14
    }
  )
print ( withPackagesLoaded(c('SoilR','R6Unit'),expr))
print(search())

res <- tryCatch(
  Model,
  error=function(e)e
)
print(res)

# for a single package one can also use 
print(eval(expr,envir=loadNamespace('SoilR')))

