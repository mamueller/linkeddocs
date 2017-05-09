
# vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
arglistStr=function(functionObject){
  ### recover the formal arguments and default values from a function
  fa  <-formals(functionObject)
  argnames=names(fa)
  fun <- function(argname){
      if (fa[[argname]]!=""){
        return(
          .widthCutter( 
            sprintf("%s=%s",argname,as.expression(fa[[argname]])),
            89
          )
        )
      }else{
        return(argname)
      }
  }
  arglistWithDefaults<-unlist(lapply(argnames,fun))
  arglistStr<-paste0(arglistWithDefaults,collapse=",\n")
  return(arglistStr)
}
