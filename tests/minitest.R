#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
require('devtools')
require('methods')
require('linkeddocs')
pkgName <- 'PrivateAndPublic'
pkgDir <- sprintf('IoTestResults_tmp/PackageTests_3.test.%s/pkg',pkgName)
devtools::install(pkgDir)

manPath <- file.path(pkgDir,'man')
if (!file.exists(manPath)){
  dir.create(recursive=TRUE,manPath)
}else{
  lapply(
     list.files(full.names = TRUE ,manPath,recursive = FALSE,patter='*.Rd'),
    unlink
  )
}


results <- callWithPackageEnv(pkgDir,documentS4Classes,pkgDir,manPath)
#print(results)
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
