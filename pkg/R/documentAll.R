
## vim:set ff=unix expandtab ts=2 sw=2:
# This function is supposed to be run in an environment where the package to be documented is loaded
documentAll<- function(
  pkgEnv,
  results,
  pkgDir,
  manPath,
  fqPkgName
  ){
  require(tools)
  require(digest)
  docObjects <- c()

  ################################### 
  #### document S4Classes

  docObjects <- c(docObjects,
     documentS4Classes(pkgEnv, results, pkgDir)) 

  #stop('mmmm')

  ################################### 
  #### document generic functions

	exportedGens<-getGenerics(fqPkgName) 
  # includes ?internal_generic like  [ [[ $ ..

	exportedGenNames<-getGenerics(where=pkgEnv) 
  # includes ?internal_generic
  # like  [ [[ $ but only if the package defines mehtods for them
  # we find all generics for which at least one method is defined 
  # by the package # because we have to document it if it is exported

	GensWithDocMethods<-exportedGenNames[unlist(sapply(exportedGenNames,GenHasAnyMethodWithSrc,pkgDir))]
	
  docObjects <- c(docObjects,
    documentS4GenericsAndMethods( pkgEnv,results, pkgDir, GensWithDocMethods))
  

  ################################### 
  #### document non generic functions
  
  funcs <- findAllFunctions(pkgEnv)
  #### document automatically created classGeneratorfunctions
  autoConstrNames <-  autoConstructorNames(funcs)
  docObjects <- c(docObjects,
    documentS4ClassGeneratorFunctions( pkgEnv, pkgDir, autoConstrNames))

  namesOfNormalFuncs<-setdiff(
    names(funcs),
    union(exportedGenNames,autoConstrNames)
  )
  docObjects <- c(
    docObjects,
    documentNormalFuncs( pkgEnv, pkgDir, namesOfNormalFuncs))

  #### warn about objects that are not documented yet 
  remaining_objects<-setdiff(names(ls(pkgEnv)),names(funcs))

  ################################### 
  #### write Rd files for docObjects

  # list all names
  defaultFileNames <- lapply(
    docObjects,
    function(obj){defaultRdFileName(obj)})
  # provide uniqueness and remove forbidden patterns
  uniqueNames <- fixPackageFileNames(defaultFileNames)
  # write with the unique name
  for (i in seq_along(defaultFileNames)){
    obj <- docObjects[[i]]
    path <- file.path(manPath,sprintf('%s.Rd',uniqueNames[[i]]))
    write_Rd_file(obj,fn=path)
  }

  #### copy the manMan Files back
  manManPath <- file.path(manPath,'manMan')
  if (file.exists(manManPath)){
    for (fn in list.files(manManPath)){
  	  file.copy(file.path(manManPath,fn),manPath,recursive=TRUE,overwrite=TRUE)
    }
  }
  docObjects
}
