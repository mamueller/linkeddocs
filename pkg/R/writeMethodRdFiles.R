#
# vim:set ff=unix expandtab ts=2 sw=2:
writeMethodRdFiles <- function# add files that document a single method 
  ### extract the method specific documentation and write a method specific Rd file.
  ### Note that such a file would not be created by package.skeleton
(e,pkgDir,path,exprs,code,desc,inlinedocs.documentNamespaceOnly=FALSE,inlinedocs.exampleDir,inlinedocs.exampleTrunk,nsi){
  #first create the doclinks from the source code
  docLinks <- methodDocLinks(exprs,code,e)
  ## now create the actual documentation list
  # for possibly needed recursion provide a list of 
  objs <- sapply(ls(e),get,e,simplify=FALSE)
  doc.names <- names(objs)
  res=list()
  for ( nn in names(docLinks) ){
    dL=docLinks[[nn]]
    if ( dL@created == "setMethod" ){
      S4Method.docs <- extract.docs.setMethod(dL,e,inlinedocs.exampleDir,inlinedocs.exampleTrunk)
      docname <- dL@name
      if ( is.null(res[[docname]]) ){
        res[[docname]] <- S4Method.docs
        doc.names <- c(doc.names,docname)
      } else {
        stop(nn," appears as both S4 method and some other definition")
      }
    }
  }
  all.done <- FALSE
  while ( !all.done ){
    res1 <- sapply(doc.names,inherit.docs,parsed=docLinks,res=res,simplify=FALSE)
    
    all.done <- identical(res1,res)
    res <- res1
  }
  MethodsInSrc <-nsi[["documentableMeths"]] 
  #pp("MethodsInSrc",environment())
  #####################################
  
  genericFuncNames<-names(MethodsInSrc)
  for(genName in genericFuncNames){
      gens<-nsi[["gens"]]
      methsBySig=MethodsInSrc[[genName]]
      #for ( meth in methsBySig){
      for ( i in 1:length(methsBySig)){
      	#gen=getGeneric(genName,e)
      	#sig=matchSignature(methSig(meth),gen,e)
        meth<-methsBySig[[i]]
        gen<-gens[[genName]]
        sig<-methSig(meth)
      	docSig<-matchSignature(sig,gen)

      	src=as.character(getSrcref(unRematchDefinition(meth)))
      	srcDir=getSrcDirectory(unRematchDefinition(meth))
      	srcFile=getSrcFilename(unRematchDefinition(meth),full.names=TRUE)
        docName <- methodDocName(genName,docSig)
        N<-methodDocName(genName,sig)
        # renaming of operator files like package.skeleton does
        #Nme <- fixPackageFileNames(N)
        Nme <-fixPackageFileNames(paste(genName,"-method_",toString(i),sep=""))
        #pp("N",environment())
        #pp("Nme",environment())
    
    	  methodDocList=res[[docName]]

        fff <- eval(parse(text=str_split(src,"\n")))
        #f)ff <- meth
        mdo=methodDocObject(l=methodDocList,name=N,genName=genName,sig=sig,src=src,functionObject=fff)
    
        p=file.path(path,paste(Nme,".Rd",sep=""))
            write_Rd_file(obj=mdo,fn=p)
    	}
       
  }
  
}
