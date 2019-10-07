
# vim:set ff=unix expandtab ts=2 sw=2:
evalWithPackageLoaded <- function(pkgName,expr){
	#res <- eval(expr,envir=loadNamespace(pkgName)) 
  succ <- require(pkgName,character.only=TRUE)
  if (succ){
    myEnv=new.env()
    res <- eval(expr,envir=myEnv)
    detach(sprintf('package:%s',pkgName),character.only=TRUE,unload=TRUE)
  }
  return(res)
}
