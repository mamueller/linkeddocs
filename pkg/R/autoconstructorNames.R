
# vim:set ff=unix expandtab ts=2 sw=2:
autoConstructorNames <- function(funcs){
  # foo <- setClass(Class=bar ... statements create a function fooBar that 
  # acts as a constructor for class 'bar'
  # we have to treat those functions with special care
  boollist <-unlist(lapply(funcs,function(func){inherits(func,'classGeneratorFunction')}))
  pp('boollist')
  if(any(boollist)){
    autoConstructors<-funcs[boollist]
    autoConstructorNames  <- names(autoConstructors)
  }else{
    autoConstructorNames <- NULL
  }
  autoConstructorNames 
}
