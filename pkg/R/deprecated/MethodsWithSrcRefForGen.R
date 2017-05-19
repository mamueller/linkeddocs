# deprecated becuase obsolete
# findMethods(genName,where=asNamespace(pkgName))
# does the same thing.

# vim:set ff=unix expandtab ts=2 sw=2:
MethodsWithSrcRefForGen=function(genName,pkgDir){ 
  all <- findMethods(genName)
  l=all[sapply(all,MethodHasSrc,pkgDir)]
  l
}
	
