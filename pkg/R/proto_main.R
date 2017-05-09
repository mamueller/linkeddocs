
# vim:set ff=unix expandtab ts=2 sw=2:
 pkgInfoFromInstall<-function(pkgDir){
	pkgName<-packageDescription(pkgDir,".",fields="Package")
  privatePackageLib<-file.path("tmp",pkgName)
  if (!dir.exists(privatePackageLib)){
    dir.create(privatePackageLib,recursive=TRUE)
  }
	install.packages(pkgDir,lib=privatePackageLib,repo=NULL,INSTALL_opts="--with-keep.source")
	library(pkgName,lib.loc=privatePackageLib,character.only=TRUE)
  nslist<-parseNamespaceFile(pkgName,package.lib=privatePackageLib)
  return(nslist)
}
