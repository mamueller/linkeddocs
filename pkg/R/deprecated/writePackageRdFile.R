
# vim:set ff=unix expandtab ts=2 sw=2:
#################################################################
writePackageRdFile <- function(pkgDir,name,path,desc){
   
   p=file.path(".")
   mm_promptPackage(
     package=name, 
     filename = file.path(path, sprintf("%s-package.Rd", name)), 
     lib.loc = p,
     description=desc
   )

}
