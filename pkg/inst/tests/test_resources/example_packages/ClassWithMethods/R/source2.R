### Since this class is exported in the Namespace file you can inherit from it
### but nethertheless the method for "hiddenGeneric" with this class as
### a signature will not be visible
setClass(# an Exposed  class
   Class="ExposedClass",
   representation=representation(
        times="numeric"
   )
)
#------------------------------------------------
# the next method should not appear in the help 
# because the generic function is not exported
setMethod(
   f= "hiddenGeneric",
   signature="ExposedClass",
   definition=function#short title
   ### short description
   (object){
       return(object@times)
     }
)
#------------------------------------------------
# the next method should appear in the help 
# because the generic function is exported
setMethod(
   f= "exposedGeneric",
   signature="ExposedClass",
   definition=function#short title
   ### short description
   (object){
       return(object@times)
     }
)
#---------------------------------------------------------------
setMethod(### for [ (and others [[,$...) documentation is required although the method is not explicitly exported
  f="[",
  signature(x="ExposedClass",i="character",j="missing",drop="missing"),
  definition=function# fake [ method
  ### We fake the behavior of the [] operator to provide a more
  ### R like interface.
  (
  x, ##<< the soil model
  i  ##<< the name of the method
  ){
  2 ##<< the result of the get[[PropertyName]] method
  }
)

