# vim:set ff=unix expandtab ts=2 sw=2:
GenHasSrc<-function(genName,results,pkgDir,env){
    gen<-getGeneric(genName,where=env)
    #srcRef <- getSrcref(gen) 
    srcRef <- findGenericSrcRef(results,genName)
    if(!is.null(srcRef)){
      res<-pkgRPath(pkgDir)==getSrcDirectory(srcRef)
    }else{
      res<-FALSE
    }
    res
}

