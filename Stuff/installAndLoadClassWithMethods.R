require('devtools')
install('pkg')
pkgName<-'ClassWithMethods'
require(pkgName,character.only = TRUE)
instPkgPath<-find.package(pkgName)
exDir<-file.path(instPkgPath,"examples")
exampleFileNames<-list.files(exDir)
file.show(file.path(exDir,"example.exposedGeneric-method-45e6a2d5c738352283ff20e4fcd6c650.R"))
