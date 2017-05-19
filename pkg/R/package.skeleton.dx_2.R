
# vim:set ff=unix expandtab ts=2 sw=2:
### This function will eventually replace package.skeleton.dx
### The return values are only uses for testing 
package.skeleton.dx_2<-function(pkgDir){
  require(devtools)
  descfile <- file.path(pkgDir,"DESCRIPTION")
  desc<-extract_description(descfile)
  pkgName <- as.character(read.dcf(descfile,fields=c('Package')))
   
  all<-devtools::load_all(pkgDir,export_all=F)
  #print(all)
  env <- all$env
	gens<-getGenerics(where=env) 
  pp('pkgDir')
	GensWithSrc<-gens[unlist(sapply(gens,GenHasSrc,pkgDir,env))]
	
  gens2<-getGenerics(sprintf("package:%s",pkgName)) 
	GensWithDocMethods <- getGenerics(asNamespace(pkgName)) 

  
  documentableMeths <-  documentableMeths(gens2,pkgName)
  write_Rd_file(documentableMeths[['[']][[1]])
  
  
  
  return(
    list(
      documentableMeths   = documentableMeths,
      gens                = gens,
      gens2               = gens2,
      GensWithSrc         = GensWithSrc,
      GensWithDocMethods  = GensWithDocMethods
    )
  )
}
