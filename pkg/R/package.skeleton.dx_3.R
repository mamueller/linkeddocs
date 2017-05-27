
# vim:set ff=unix expandtab ts=2 sw=2:
package.skeleton.dx_3<-function(pkgDir){

  require(tools)
  privatePackageLib<-file.path(pkgDir,"tmp",'lib')
  pkgR<-normalizePath(file.path(pkgDir,'R'))
  manPath <- file.path(pkgDir,'man')
  manManPath <- file.path(manPath,'manMan')
  if (!file.exists(manManPath)){
    dir.create(recursive=TRUE,manManPath)
  }
 
  classInSig <- function(g,  cl) {
      cl %in% dm[[g]]@signatures
  }
	### get at the src of a method given as  an MethodDefinition object
	methSrc=function(MethodDefinition){
		getSrcref(unRematchDefinition(MethodDefinition))
	}
	
	
  if (!dir.exists(privatePackageLib)){
    dir.create(privatePackageLib,recursive=TRUE)
  }
  oldLibs <- .libPaths()[]
  on.exit(.libPaths(oldLibs))
	install.packages(pkgDir,lib=privatePackageLib,repos=NULL,INSTALL_opts="--with-keep.source", type="source",quiet=TRUE)
	pkgName<-as.character(read.dcf(file=file.path(pkgDir,'DESCRIPTION'),fields='Package'))
	library(pkgName,lib.loc=privatePackageLib,character.only=TRUE,quietly=TRUE)
  fqPkgName <- sprintf("package:%s",pkgName)
  # Every package has two environments 
  # 1.) the package environment is where its (exported) function are bound to
  pkgEnv <- as.environment(fqPkgName) 
  # 2.) the package Namespace environment which encloses its functions  and defines 
  # where the functions find their values.
  # http://adv-r.had.co.nz/Environments.html#function-envs

  pkgNsEnv <- asNamespace(pkgName) #
  
	exportedGens<-getGenerics(fqPkgName) #includes ?internal_generic like  [ [[ $ ..
  pp('exportedGens')
	exportedGenNames<-getGenerics(where=pkgEnv) #includes ?internal_generic like  [ [[ $ but only if the package defines mehtods for them
  pp('exportedGenNames')
  #print(exportedGenNames2)

  # we find all generics for which at least one  method is defined by the package
  # because we have to document it if it is exported
	GensWithDocMethods<-exportedGenNames[unlist(sapply(exportedGenNames,GenHasAnyMethodWithSrc,pkgDir))]
  pp('GensWithDocMethods')

	
	for (genName in GensWithDocMethods){
		meths<- findMethods(genName,where=pkgEnv)
    i <- 1
    for (m in meths){
      pp('m')
      pe(quote(class(m)))
      i <- i+1
      Nme <-fixPackageFileNames(paste(genName,"-method_",toString(i),sep=""))
      p=file.path(manPath,paste(Nme,".Rd",sep=""))
      write_Rd_file(m,p)
    }
    # not all the generics our package defines mehtods for are also defined by the 
    # package. Some like [, [[, $ have been there before.
    # Only the generics in the package need their own Rd file
    if (GenHasSrc(genName,pkgDir,pkgEnv)){
      pp('genName')  
      pe(quote(class(getGeneric(genName))))  
      write_Rd_file(
        getGeneric(genName),
        file.path(manPath,paste(fixPackageFileNames(genName),".Rd",sep=""))
      )
    }
	}

  # find all functions in the package
  objectNames<-ls(sprintf("package:%s",pkgName))
  exportedGenNames<-unlist(as.list(exportedGenNames))
  funcs<-list()
  for (fn in objectNames){
    f<-eval(as.symbol(fn))
    if (is.function(f)){
      funcs[[fn]]<-f
      }
  }
  remaining_objects<-setdiff(objectNames,names(funcs))

  #pe(quote(),environment())
  exportedClassNames<-getClasses(as.environment(sprintf("package:%s",pkgName)))
  nonGenericNames<-setdiff(names(funcs),exportedGenNames)
  nonGenerics<-funcs[nonGenericNames]
	detach(sprintf("package:%s" ,pkgName),unload=TRUE,character.only=TRUE)
}
