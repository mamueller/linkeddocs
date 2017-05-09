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
  if(inlinedocs.documentNamespaceOnly){
    MethodsInSrc <-nsi[["documentableMeths"]] 
  }else{
    MethodsInSrc <- documentableMeths(e,pkgDir)
  }
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
       
        ## write docu to a list
        #l=as.list(prompt(fff,filename=NA))
        #l[["name"]]=paste("\\name{",N,"}",sep="")
        #l[["title"]]= paste("\\title{",N,"}",sep="")
        #l[["aliases"]]=paste("\\alias{",N,"}",sep="")
        ##l[["aliases"]]=paste("\\alias{",N,"}",sep="")

        ### overwrite the usage section (see below for reasons)
        #argString<-toString(names(sig))
        #sigString<-toString(as.vector(sig))
        #S4String<-paste0("\\S4method{",genName,"}{",sigString,"}(",argString,")")
        #usageString<-paste0("\\usage{",S4String,"}")
        #l[["usage"]]=usageString

        ## in every section of the list replace fff by the name of the Generic
        ### Note:
        ### The usage section from the R generated method description causes a warning
        ### about a missing alias.
        ### For example:
        ### If we have a generic function exampleGen with a mehthod for Class "A"
        ### the "prompt" call and subsequent renameing above would set:
        ### l[["alias"]] <- "exampleGen-method-#A"   while the usage section
        ### l[["usage"]] <- "exampleGen(object)"
        ### If this was written to the Rd file later R CMD check will WARN that it
        ### did not find the name used in the usage section of the file as an alias in the same file.
        ### "R CMD check" usually expects a usage section of the kind 
        ### l[["usage"]] == "exampleGen-method-#A(object)" or
        ### l[["usage"]] == "Alias1" or
        ### l[["usage"]] == "Alias2" ...

        ### But for a mehthod of a Generic the usage section should contain the name of that Generic.
        ### The temptation is to set the alias to the name of the Generic,
        ### but this would be a disaster:
        ### A user typing ?exampleGen would get either:
        ### - the documentation of the generic function "exampleGen" or 
        ### - the documentation of  one of the first method implementig "exampleGen" 
        ### - the documentation of  one of the second method implementig "exampleGen" 
        ### - ...
        ### The last file sourced by R's help system containing an \alias{"exampleGen""}
        ### would be shown.
        ### So we could either ignore the (unfounded) WARNING or remove the usage section completely
        ### from the method decription (which I will do here as default)

        ##l <- l[setdiff(names(l),"usage")]
        ## we can also remove the alias since we do not need it.
        ##l <- l[setdiff(names(l),"aliases")]
        ## now we actually replace the empty sections by the content we have extracted
        ##for (n in names(l)){
        ##  
        ##}
        #p=file.path(path,f)
        #cat(mask_special_Rd_characters(unlist(l)), file = p, sep = "\n")
      	#}
  }
  
}
