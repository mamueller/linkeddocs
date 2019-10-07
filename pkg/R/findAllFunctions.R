
# vim:set ff=unix expandtab ts=2 sw=2:
findAllFunctions <- function(pkgEnv){
    objectNames<-ls(pkgEnv)
    funcs<-list()
    for (fn in objectNames){
      f<-eval(as.symbol(fn),pkgEnv)
      if (is.function(f)){
        funcs[[fn]]<-f
        }
    }
    funcs
}
