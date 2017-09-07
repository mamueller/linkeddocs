
############################################################
hiddenClasses <- function(env,pkgDir){
  setdiff(allClasses(env),exportedClasses(pkgDir))
}


