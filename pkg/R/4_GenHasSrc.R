# vim:set ff=unix expandtab ts=2 sw=2:
GenHasSrc<-function(genName,pkgDir,env){
    gen<-getGeneric(genName,where=env)
    srcDir<-getSrcDirectory(gen)
    
    if (length(srcDir)>0){
      res<-pkgRPath(pkgDir)==srcDir
    }else{
      res<-FALSE
    }
    res
}
