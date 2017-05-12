#!/usr/bin/Rscript 
# vim:set ff=unix expandtab ts=2 sw=2:
pkgName<-"linkeddocs"
if(is.element(pkgName,installed.packages())){
  remove.packages(pkgName)
}
install.packages(file.path("../../"),repo=NULL,INSTALL_opts="--with-keep.source")
library(linkeddocs,quiet=TRUE)
library(devtools,quiet=TRUE)
prefix<-"/home/mm/linkeddocs/pkg/inst/tests/IoTestResults_tmp"
options(warn=1)
#testName<-"PackageTests.test.noMethodRdFilesForHiddenMethods"
testName<-"PackageTests.test.MethodLinksForClass"
#testName<-"PackageTests.test.abbriviatedSignature"
#testName<-"PackageTests.test.GenericWithDotDotDotArgumets"
#testName<-"PackageTests.test.SoilR"
#testName<-"PackageTests.test.OverloadedIndexedAssingment"

path<-file.path(prefix,testName,"pkg")
print(path)
package.skeleton.dx(path)
#check(path,document=FALSE,quiet=TRUE)
check(path,document=FALSE)
#cat(paste(readLines(file.path(path,"man","ExposedClass-class.Rd")),collapse="\n"))
#cat(paste(readLines(file.path(path,"man","exposedGeneric.Rd")),collapse="\n"))
