
## vim:set ff=unix expandtab ts=2 sw=2:
writeMethodRdFiles_fromSrcRef <- function(env){
	#exportedGens<-getGenerics(sprintf("package:%s",pkgName)) #includes ?internal_generic like  [ [[ $ ..
	exportedGens<-getGenerics(where=env) #includes ?internal_generic like  [ [[ $ ..
  print(exportedGens)
	GensWithDocMethods<-exportedGens[unlist(sapply(exportedGens,GenHasAnyMethodWithSrc,env=env))]
  print('###########################')
  print(GensWithDocMethods)
	documentableMeths<-list()
}
