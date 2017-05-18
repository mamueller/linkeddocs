
# vim:set ff=unix expandtab ts=2 sw=2:
MethodsWithSrcRefForGen=function(genName,e=parent.env()){ 
  l=findMethods(genName)[sapply(findMethods(genName),MethodHasSrc)]
  l
}
	
