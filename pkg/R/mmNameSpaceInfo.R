
# vim:set ff=unix expandtab ts=2 sw=2:
mmNameSpaceInfo<-function(pkgDir){
  require(tools)
  privatePackageLib<-file.path(pkgDir,"tmp",'lib')
  pkgRPath<-normalizePath(file.path(pkgDir,'R'))
 
  classInSig <- function(g,  cl) {
      cl %in% dm[[g]]@signatures
  }
	### get at the src of a method given as  an MethodDefinition object
	methSrc=function(MethodDefinition){
		getSrcref(unRematchDefinition(MethodDefinition))
	}
	
	GenHasSrc<-function(genName,pkg){
    gen<-getGeneric(genName)
    srcDir<-getSrcDirectory(gen)
    
    if (length(srcDir)>0){
      res<-pkgRPath==srcDir
    }else{
      res<-FALSE
    }
    #pp("res",environment())
		res
	}
	
	### This function tells if we can find a src reference for this method
	MethodHasSrc<-function(MethodDefinition){
    srcDir<-getSrcDirectory(MethodDefinition)
    #pp('srcDir')
    #pp('pkgRPath') 
    if (length(srcDir)>0){
      res<-pkgRPath==srcDir
    }else{
      res<-FALSE
    }
    #pp("MethodDefinition",environment())
    #pp("res",environment())
		res
	}
	
	
	### Not all methods for a Generic are defined in the src we want to document.
	### This function helps to find the methods we want.
	MethodsWithSrcRefForGen=function(genName){ 
	  l=findMethods(genName)[sapply(findMethods(genName),MethodHasSrc)]
	  l
	}
	
	
	### function to check if we have a src reference for any of the methods of this generic
	GenHasAnyMethodWithSrc=function
	(genName,env){
	  methDefs <- findMethods(genName)
    #pp('genName')
	  return(
	  	any(
			  sapply(
	    			methDefs,
	    			MethodHasSrc
			  )
		  )
	  )
	}
  if (!dir.exists(privatePackageLib)){
    dir.create(privatePackageLib,recursive=TRUE)
  }
  oldLibs <- .libPaths()[]
  on.exit(.libPaths(oldLibs))
 # .libPaths(privatePackageLib)
 # devtools::install(pkgDir,keep_source=T)
	install.packages(pkgDir,lib=privatePackageLib,repos=NULL,INSTALL_opts="--with-keep.source", type="source",quiet=TRUE)
	pkgName<-as.character(read.dcf(file=file.path(pkgDir,'DESCRIPTION'),fields='Package'))
	library(pkgName,lib.loc=privatePackageLib,character.only=TRUE,quietly=TRUE)
  nslist<-parseNamespaceFile(pkgName,package.lib=privatePackageLib)
  #pe(quote(nslist$exportClasses))
	exportedGens<-getGenerics(sprintf("package:%s",pkgName)) #includes ?internal_generic like  [ [[ $ ..
  for (eg in exportedGens){
    #pp('eg')
    v<-GenHasAnyMethodWithSrc(eg)
    #pp('v')
  }
  #pp('exportedGens')
	GensWithDocMethods<-exportedGens[unlist(sapply(exportedGens,GenHasAnyMethodWithSrc))]
  print('###########################')
  #pp('GensWithDocMethods')
	GensWithSrc<-exportedGens[unlist(sapply(exportedGens,GenHasSrc))]
	
	documentableMeths<-list()
  gens_defined_by_package<-list()
  gens_defined_previously<-list()
	for (genName in GensWithDocMethods){
    if (is.element(genName,GensWithSrc)){
        gens_defined_by_package[[genName]]<-getGeneric(genName)
      }else{
        gens_defined_previously[[genName]]<-getGeneric(genName)
    }
		documentableMeths[[genName]]<-MethodsWithSrcRefForGen(genName)
	}
  gens<-c(gens_defined_by_package,gens_defined_previously)

  # find all functions in the package
  objectNames<-ls(sprintf("package:%s",pkgName))
  exportedGenNames<-unlist(as.list(exportedGens))
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
	remove.packages(pkgName,lib=privatePackageLib)
  return( list(
    documentableMeths       = documentableMeths,
    gens_defined_by_package = gens_defined_by_package,
    gens_visible_in_pkg     = exportedGens,
    gens_defined_previously = gens_defined_previously,
    gens                    = gens,
    non_gens                = nonGenerics,
    exportedClassNames      = exportedClassNames
    #exportedClassNames      = nslist$exportClasses
    ))
}
