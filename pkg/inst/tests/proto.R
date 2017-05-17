source('helpers.R')
#source('../../R/mmNameSpaceInfo.R')
pkgDir<-'../../../pkg'
privatePackageLib<-file.path("tmp",'lib')
install.packages(pkgDir,lib=privatePackageLib,repos=NULL,INSTALL_opts="--with-keep.source", type="source",quiet=TRUE)
pkgName<-as.character(read.dcf(file=file.path(pkgDir,'DESCRIPTION'),fields='Package'))
library(pkgName,lib.loc=privatePackageLib,character.only=TRUE,quietly=TRUE)
library(linkeddocs)
nsi <- mmNameSpaceInfo(pkgDir)
library(devtools)
pkgDir<-'IoTestResults_tmp/PrototypeTests.test.selfload/pkg/'
all<-devtools::load_all(pkgDir)
print((mmNameSpaceInfo))
