
# vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
arglistStr=function(functionObject){
  ### recover the formal arguments and default values from a function
  fa  <-formals(functionObject)
  argnames=names(fa)
  fun <- function(argname){
      # default
      s <- sprintf("%s=%s",argname,as.expression(fa[[argname]]))
      if(!is.null(fa[[argname]])){
        # the next condition can only be checked if the default value
        # is not NULL
        if (fa[[argname]]==""){
          s <- argname
        }
      }
      return( .widthCutter( s , 89))
  }
  arglistWithDefaults<-unlist(lapply(argnames,fun))
  arglistStr<-paste0(arglistWithDefaults,collapse=",\n")
  return(arglistStr)
}
