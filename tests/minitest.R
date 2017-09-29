#!/usr/bin/Rscript
source('helpers.R')
require('devtools')
require('methods')
require('linkeddocs')
pkgName <- 'PrivateAndPublic'
pkgDir <- sprintf('IoTestResults_tmp/PackageTests_3.test.%s/pkg',pkgName)
linkeddocs::package.skeleton.dx_3(pkgDir)
#dnse <-  devtools::load_all(pkgDir)
##env=dnse$env
##print(getClasses(where=env))
#pkgR<-normalizePath(file.path(pkgDir,'R'))
#codeFiles <- list.files(pkgR,full.names=TRUE)
#exprs <- c()
#
#for (fn in codeFiles){
#  exprs <- c(exprs,parse(fn,keep.source=TRUE))
#}
#
#source_env <- new.env()
#for (expr in exprs){
#  eval(expr,envir=source_env)
#}
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
