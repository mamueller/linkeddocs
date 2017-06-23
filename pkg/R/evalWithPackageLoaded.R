
# vim:set ff=unix expandtab ts=2 sw=2:
evalWithPackageLoaded <- function(PkgName,expr){
	res <- eval(expr,envir=loadNamespace(pkgName)) 
  return(res)
}
