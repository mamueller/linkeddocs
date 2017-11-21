
# vim:set ff=unix expandtab ts=2 sw=2:
#-------------------------------------------------------------------------
arglistStr=function(functionObject){
  ### recover the formal arguments and default values from a function
  fa  <-formals(functionObject)
  argnames=names(fa)
  fun <- function(argname){
      if(!is.null(fa[[argname]])){
        if (fa[[argname]]!=""){
              s <- as.expression(fa[[argname]])
        }else{
          s <- ' '
        }
      }else{
        s <- 'NULL'
      }
      return( .widthCutter( sprintf("%s=%s",argname,s) , 89))
  }
  arglistWithDefaults<-unlist(lapply(argnames,fun))
  arglistStr<-paste0(arglistWithDefaults,collapse=",\n")
  return(arglistStr)
}
