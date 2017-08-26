
# vim:set ff=unix expandtab ts=2 sw=2:
package.skeleton.dx_3<-function(pkgDir){

  require(tools)
  require(digest)
  privatePackageLib<-file.path(pkgDir,'..','tmp','lib')
  pkgR<-normalizePath(file.path(pkgDir,'R'))
  codeFiles <- list.files(pkgR,full.names=TRUE)
  code<- ''
  for (fn in codeFiles){code <- append(code,readLines(fn))}

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
  
  #devtools::install(pkgDir,keep_source=TRUE)
	#library(pkgName,character.only=TRUE,quietly=TRUE)
  #all<-devtools::load_all(pkgDir,export_all=FALSE)

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
      write_Rd_file(m,p)
      i <- i+1
    }
    # not all the generics our package defines mehtods for are also defined by the 
    # package. Some like [, [[, $ have been there before.
    # Only the generics in the package need their own Rd file
    if (GenHasSrc(genName,pkgDir,pkgEnv)){
      #pe(quote(class(getGeneric(genName))))  
      write_Rd_file(
        getGeneric(genName),
        file.path(manPath,paste(fixPackageFileNames(genName),".Rd",sep=""))
      )
    }
	}

  
  #### document S4 classes
  exportedClassNames<-getClasses(pkgEnv)
  for (eCName in exportedClassNames){
      filename <- file.path(manPath,sprintf("%s-class.Rd",eCName))
      # We have to find the part of the source code since R doen not provide a srcref for class definitions
      write_Rd_file(obj=getClass(eCName),fn=filename,code=code)
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
  nonGenericNames<-setdiff(names(funcs),exportedGenNames)
  list0 <- fixPackageFileNames(nonGenericNames)
  names(list0) <- nonGenericNames
  nonGenerics<-funcs[nonGenericNames]
  sapply(
    nonGenericNames, 
    function(funcName) {
      obj<-get(funcName)
      fn <- file.path(manPath, paste(list0[[funcName]],".Rd",sep=""))
      srcRef <- utils::getSrcref(obj)
      codeText <- as.character(srcRef,useSource=T)
      code <- readLines(getSrcFilename(obj,full.names=TRUE))
      pos <- utils::getSrcLocation(srcRef)
      leadingComments <- ''
      pos <- pos-1
      line <- code[pos]
      while(grepl('^\\s*###',line) && pos >1){
        #codeText<- c(line,codeText)
        leadingComments<- c(line,leadingComments)
        pos <- pos-1
        line <- code[pos]
      }
      leadingDesc <- gsub("^[ \t(,#]*", "",leadingComments)
      leadingDesc <- leadingDesc[!grepl('^ *$',leadingDesc)]
      l <- extract.xxx.chunks(codeText)
      pl <- prefixed.lines(codeText)
      pl[['description']] <- append(leadingDesc,pl[['description']])
      #l[['description']] <- append(pl[['description']],l[['description']])
      l <- combine(l,pl)
      tit_list <- title.from.firstline(codeText)
      #fixme mm:
      # at the moment title.from.firstline(codeText) returns a list
      # which is unnecessary, it should be changed to a character vector or NULL
      # as soon as the old version is not needed any more
      if(is.null(tit_list[['title']])){tit_list <- funcName}
      l[['title']] <- tit_list
      fdo=functionDocObject(name=funcName,l=l,functionObject=obj,src=codeText)
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
