
require('devtools')
pkgName <- 'AutoConstructor'
pkgDir <- sprintf('IoTestResults_tmp/PackageTests_3.test.%s/pkg',pkgName)
devtools::install(pkgDir)
require(pkgName,character.only=TRUE)
fqPkgName <- sprintf("package:%s",pkgName)
pkgEnv <- as.environment(fqPkgName) 
pkgNsEnv <- asNamespace(pkgName) #
exportedClassNames<-getClasses(pkgEnv)
print(exportedClassNames)
acn<-getClasses(pkgNsEnv)
print(acn)
detach(fqPkgName,character.only=TRUE,unload=TRUE)
source_env <- devtools:::create_ns_env(pkgDir)
exportedClassNames<-getClasses(pkgEnv)
print(exportedClassNames)

