
# vim:set ff=unix expandtab ts=2 sw=2:
documentS4Classes <- function(pkgEnv,results,pkgDir,manPath){
	exClNs <- getClasses(pkgEnv)
	pe(quote(exClNs))
	for (clname in exClNs){
      filename <- file.path(manPath,sprintf("%s-class.Rd",clname))
      # We have to find the part of the source code since R doen not provide a srcref for class definitions
      sr <-  findClassSrcRef(results,clname) 
      pe(quote(class(sr)))
      obj <- get_docObject(getClass(clname),pkgDir=pkgDir,srcref=sr)
      #pp('obj')
      write_Rd_file(obj,fn=filename)
    

	}
	
}
