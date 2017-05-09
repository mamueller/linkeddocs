#!/usr/bin/env bash
# vim:set ff=unix expandtab ts=2 sw=2:
${soilrRepoBase}/scripts/InstallSourcePackages.R ~/linkeddocs/pkg ~/R6Unit/pkg

## reinstall inlinedocs
#pkgName<-"inlinedocs"
#if(is.element(pkgName,installed.packages())){
#  remove.packages(pkgName)
#}
#install.packages(file.path(),repo=NULL,INSTALL_opts="--with-keep.source")
## reinstall R6Unit
#pkgName<-"R6Unit"
#if(is.element(pkgName,installed.packages())){
#  remove.packages(pkgName)
#}
#install.packages(file.path("~",pkgName,"pkg"),repo=NULL,INSTALL_opts="--with-keep.source")
#
#require(R6Unit,quiet=TRUE)
#require(inlinedocs,quiet=TRUE)

