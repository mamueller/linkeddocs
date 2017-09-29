#!/usr/bin/Rscript
source('helpers.R')
require('devtools')
require('methods')
require('linkeddocs')
pkgName <- 'PrivateAndPublic'
pkgDir <- sprintf('IoTestResults_tmp/PackageTests_3.test.%s/pkg',pkgName)
if (devtools:::is_loaded(pkgDir)){unload(pkgDir)}
source_env <- devtools:::create_ns_env(pkgDir)
#linkeddocs::package.skeleton.dx_3(pkgDir)
#dnse <-  devtools::load_all(pkgDir)
##env=dnse$env
##print(getClasses(where=env))
pkgR<-normalizePath(file.path(pkgDir,'R'))
codeFiles <- list.files(pkgR,full.names=TRUE)
results <- list()
j=1
for (fn in codeFiles){
  lines <- readLines(fn)
  sf <- srcfile(fn)
  exprs <- parse(text=lines,srcfile=sf,keep.source=TRUE)
  #print(sprintf('getParseData=%s',getParseData(exprs)))
  n <- length(exprs)
  for (i in seq_len(n)){
    expr <- exprs[[i]]
    res <- eval(expr,source_env)
    results[[j]] <- list()
    results[[j]][['res']] <- res
    results[[j]][['srcRef']] <- getParseData(expr)
    j=j+1
  }
}
#results
#devtools::install(pkgDir)
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
