
# vim:set ff=unix expandtab ts=2 sw=2:
### This function will eventually replace package.skeleton.dx
### The return values are only uses for testing 
package.skeleton.dx_2<-function(pkgDir){
  require(devtools)
  descfile <- file.path(pkgDir,"DESCRIPTION")
  #desc<-extract_description(descfile)
  pkgName <- as.character(read.dcf(descfile,fields=c('Package')))
  fqPkgName <- sprintf("package:%s",pkgName)
  #ns <-  asNamespace(pkgName)
  all<-devtools::load_all(pkgDir,export_all=FALSE)
  pkgEnv <- as.environment(fqPkgName) 
  #print(all)
  env <- all$env
	gens<-getGenerics(where=env) 
	GensWithSrc<-gens[unlist(sapply(gens,GenHasSrc,pkgDir,env))]
	
  gens2<-getGenerics(fqPkgName) 
	GensWithDocMethods <- getGenerics(where=env)

  
  documentableMeths <-  documentableMeths(gens2,pkgName)
  manPath <- file.path(pkgDir,'man')
  manManPath <- file.path(manPath,'manMan')
  if (!file.exists(manManPath)){
    dir.create(recursive=TRUE,manManPath)
  }
  for (genName in names(documentableMeths)){
    g <- getGeneric(genName,pkgEnv)
    i <- 1
    for (method in documentableMeths[[genName]]){
      i <- i+1
      Nme <-fixPackageFileNames(paste(genName,"-method_",toString(i),sep=""))
      p=file.path(manPath,paste(Nme,".Rd",sep=""))
      write_Rd_file(documentableMeths[[genName]][[1]],p)
    }
  }
  
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
