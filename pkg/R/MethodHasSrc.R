
# vim:set ff=unix expandtab ts=2 sw=2:
	### This function tells if we can find a src reference for this method
	MethodHasSrc<-function(MethodDefinition,pkgDir){
    srcDir <- getSrcDirectory(MethodDefinition)
    pkgR <-pkgRPath(pkgDir) 

    if (length(srcDir)>0){
      res<- pkgR==srcDir
    }else{
      res<-FALSE
    }
		res
	}
