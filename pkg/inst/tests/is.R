#!/usr/bin/Rscript 
install.packages(file.path("../../"),repo=NULL,INSTALL_opts="--with-keep.source")
library(linkeddocs,quiet=TRUE)
library(devtools,quiet=TRUE)
prefix<-"/home/mm/linkeddocs/pkg/inst/tests/IoTestResults_tmp"
options(warn=1)
#testName<-"PackageTests.test.noMethodRdFilesForHiddenMethods"
testName_3<-"PackageTests_3.test.ClassWithMethods"
testName<-"PackageTests.test.ClassWithMethods"
#testName<-"PackageTests.test.abbriviatedSignature"
#testName<-"PackageTests.test.GenericWithDotDotDotArgumets"
#testName<-"PackageTests.test.SoilR"
#testName<-"PackageTests.test.OverloadedIndexedAssingment"
doc_and_test <- function(testName,main){
  path<-file.path(prefix,testName,"pkg")
  tmpLibPath<-file.path(prefix,testName,"tmp")
  if (file.exists(tmpLibPath)){unlink(tmpLibPath,recursive=TRUE,force=TRUE)}
  dir.create(tmpLibPath,recursive=TRUE)
  oldp <- .libPaths()
  newp <- append(tmpLibPath,oldp)
  .libPaths(newp)
  print(.libPaths())
  #main(path)
  #check(path,document=FALSE,cran=TRUE)
  .libPaths(oldp)
}
doc_and_test(testName,package.skeleton.dx)
doc_and_test(testName_3,package.skeleton.dx_3)
#cat(paste(readLines(file.path(path,"man","ExposedClass-class.Rd")),collapse="\n"))
#cat(paste(readLines(file.path(path,"man","exposedGeneric.Rd")),collapse=f\n"))
