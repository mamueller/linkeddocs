
# vim:set ff=unix expandtab ts=2 sw=2:
package.skeleton.dx_3<-function(pkgDir){
  taskFunc <- function(pkgEnv,results,pkgDir){
    require(tools)
    require(digest)
    manPath <- file.path(pkgDir,'man')
  	pkgName<-as.character(read.dcf(file=file.path(pkgDir,'DESCRIPTION'),fields='Package'))
    fqPkgName <- sprintf("package:%s",pkgName)
    
    
  	exportedGens<-getGenerics(fqPkgName) #includes ?internal_generic like  [ [[ $ ..
    #pp('exportedGens')
  	exportedGenNames<-getGenerics(where=pkgEnv) #includes ?internal_generic like  [ [[ $ but only if the package defines mehtods for them
    #pp('exportedGenNames')
    #print(exportedGenNames2)
  
    # we find all generics for which at least one  method is defined by the package
    # because we have to document it if it is exported
  	GensWithDocMethods<-exportedGenNames[unlist(sapply(exportedGenNames,GenHasAnyMethodWithSrc,pkgDir))]
  
  	
  	for (genName in GensWithDocMethods){
  		meths<- findMethods(genName,where=pkgEnv)
      i <- 1
      for (m in meths){
        #Nme <-fixPackageFileNames(paste(genName,"-method_",toString(i),sep=""))
        Nme <- uniqueMethodFileNameTrunk(m) 
        p=file.path(manPath,paste(Nme,".Rd",sep=""))
        write_Rd_file(m,p,pkgDir=pkgDir)
        i <- i+1
      }
      # not all the generics our package defines mehtods for are also defined by the 
      # package. Some like [, [[, $ have been there before.
      # Only the generics in the package need their own Rd file
      if (GenHasSrc(genName,pkgDir,pkgEnv)){
        #pe(quote(class(getGeneric(genName))))  
        write_Rd_file(
          get_docObject(getGeneric(genName),pkgDir) ,
          file.path(manPath,paste(fixPackageFileNames(genName),".Rd",sep=""))
        )
      }
  	}
    docObjects <- c()
    docObjects <- c(docObjects,documentS4Classes(pkgEnv,results,pkgDir,manPath))
  
    #### document non generic functions
    objectNames<-ls(pkgEnv)
    funcs<-list()
    for (fn in objectNames){
      f<-eval(as.symbol(fn))
      if (is.function(f)){
        funcs[[fn]]<-f
        }
    }
    # foo <- setClass(Class=bar ... statements create a function fooBar that 
    # acts as a constructor for class 'bar'
    # we have to treat those functions with special care
    boollist <-unlist(lapply(funcs,function(func){inherits(func,'classGeneratorFunction')}))
    pp('boollist')
    if(any(boollist)){
      autoConstructors<-funcs[boollist]
      autoConstructorNames  <- names(autoConstructors)
      sapply(
        names(autoConstructors),
        function(funcName){
          func <- get(funcName)
          obj <- func@.Data
          fn <- file.path(manPath, paste(funcName,".Rd",sep=""))
          fdo=autoConstructorDocObject(
            name=funcName,
            functionObject=obj,
            pkgDir=pkgDir
          )
          write_Rd_file(fdo,fn)
        }
      )
  
    }else{
      autoConstructorNames <- NULL
    }
    namesOfNormalFuncs<-setdiff(
      names(funcs),
      union(exportedGenNames,autoConstructorNames)
    )
    list0 <- fixPackageFileNames(namesOfNormalFuncs)
    names(list0) <- namesOfNormalFuncs
    nonGenerics<-funcs[namesOfNormalFuncs]
    sapply(
      namesOfNormalFuncs, 
      function(funcName) {
        obj<-get(funcName)
        pp('obj')
        fn <- file.path(manPath, paste(list0[[funcName]],".Rd",sep=""))
        fdo=functionDocObject(
          name=funcName,
          functionObject=obj,
          pkgDir=pkgDir
        )
        write_Rd_file(fdo,fn)
      }
    )
    #### warn about objects that are not documented yet 
    remaining_objects<-setdiff(objectNames,names(funcs))
  
    #### write Rd files for docObjects
    defaultFileNames <- lapply(
      docObjects,
      function(obj){defaultRdFileName(obj)}
    )
  
    uniqueNames <- fixPackageFileNames(defaultFileNames)
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
  }
  callWithPackageEnv(pkgDir,taskFunc,pkgDir)
}
