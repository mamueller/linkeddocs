
############################################################
MethodSignatureHasOnlyExportedClasses=function(MethodDefinition,env,pkgDir)
### check if all the classes in the signature are exported in the NAMESPACE file.
### This information is needed to decide which Methods we want to document in cases
### where the documentations is restricted to the exported NAMESPACE
{
  sigStr=as.character(methSig(MethodDefinition))
  hiddCls <- hiddenClasses(env,pkgDir)
  intersection <- intersect(sigStr,hiddCls)
  res <- (length(intersection)==0)
  res
}
