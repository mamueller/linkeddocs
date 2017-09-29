
# vim:set ff=unix expandtab ts=2 sw=2:
package.skeleton.dx_3<-function(pkgDir){

  require(tools)
  require(digest)
  require(devtools)
  # before we let 'require' load the package we source it 
  # since we need some srcreferences that R does 
  # not provide 
  # This will make ALL classes and functions 
  # available not only those that are exported
  # we will later rely on the package loading 
  # mechanisms and the "package:pkgName" namespace
  # to determine what is actually visible
  pe(quote(devtools:::is_loaded(pkgDir)))
  source_env <- devtools:::create_ns_env(pkgDir)
  #pp('source_env')
  privatePackageLib<-file.path(pkgDir,'..','tmp','lib')
  pkgR<-normalizePath(file.path(pkgDir,'R'))
  codeFiles <- list.files(pkgR,full.names=TRUE)
  #exprs <- c()
  for (fn in codeFiles){
    #source(fn,keep.source=TRUE)
    lines <- readLines(fn)
    exprs <- parse(text=lines,srcfile=fn,keep.source=TRUE)
    n <- length(exprs)
    for (i in seq_len(n)){
      eval(exprs[i],source_env)
    }
  }
  pe(quote(getClasses(source_env)))
  unload(pkgDir)
  pe(quote(getClasses(source_env)))
  #stop('#mmm#')
  #pp('exprs')
  #for (expr in exprs){
  #  pp('expr')
  #  eval(expr,envir=source_env)
  #}
  manPath <- file.path(pkgDir,'man')
  if (!file.exists(manPath)){
    dir.create(recursive=TRUE,manPath)
  }else{
    lapply(
       list.files(full.names = TRUE ,manPath,recursive = FALSE,patter='*.Rd'),
      unlink
    )
  }
	
  if (!dir.exists(privatePackageLib)){
    dir.create(privatePackageLib,recursive=TRUE)
  }
  oldLibs <- .libPaths()
  newp <- append(privatePackageLib,oldLibs)
  .libPaths(newp)
  pe(quote(.libPaths()))
	pkgName<-as.character(read.dcf(file=file.path(pkgDir,'DESCRIPTION'),fields='Package'))
  fqPkgName <- sprintf("package:%s",pkgName)
	#install.packages(pkgDir,lib=privatePackageLib,repos=NULL,INSTALL_opts="--with-keep.source", type="source",quiet=TRUE)
	install.packages(pkgDir,lib=privatePackageLib,repos=NULL,INSTALL_opts="--with-keep.source", type="source")
	library(pkgName,lib.loc=privatePackageLib,character.only=TRUE,quietly=TRUE)
  on.exit({
    .libPaths(oldLibs) 
    detach(fqPkgName,unload=TRUE,character.only=TRUE) 
    unlink(privatePackageLib,recursive=TRUE,force=TRUE)
    })
  

  # Every package has two environments 
  # 1.) the package environment is where its (exported) function are bound to
  pkgEnv <- as.environment(fqPkgName) 
  # 2.) the package Namespace environment which encloses its functions  and defines 
  # where the functions find their values.
  # http://adv-r.had.co.nz/Environments.html#function-envs
  pkgNsEnv <- asNamespace(pkgName) #
  
  
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

  
  #### document S4 classes
  exportedClassNames<-getClasses(pkgEnv)
  pp('exportedClassNames')
  for (eCName in exportedClassNames){
      filename <- file.path(manPath,sprintf("%s-class.Rd",eCName))
      # We have to find the part of the source code since R doen not provide a srcref for class definitions
      #write_Rd_file(obj=getClass(eCName),fn=filename,code=code)
      obj <- get_docObject(getClass(eCName),pkgDir=pkgDir,source_env=source_env)
      pp('obj')
      write_Rd_file(obj,fn=filename)
  }

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

  #### copy the manMan Files back
  manManPath <- file.path(manPath,'manMan')
  if (file.exists(manManPath)){
    for (fn in list.files(manManPath)){
  	  file.copy(file.path(manManPath,fn),manPath,recursive=TRUE,overwrite=TRUE)
    }
  }
}
