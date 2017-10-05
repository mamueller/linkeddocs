
# vim:set ff=unix expandtab ts=2 sw=2:
documentS4Classes <- function(pkgEnv,results,pkgDir){
	exClNs <- getClasses(pkgEnv)
	pe(quote(exClNs))
  return(
    lapply(
      exClNs,
      function(clname,pkgDir){
        sr <-  findClassSrcRef(results,clname) 
        obj <- get_docObject(getClass(clname),pkgDir=pkgDir,srcref=sr)
        obj
      },
      pkgDir
    )
  )
}
