#
# vim:set ff=unix expandtab ts=2 sw=2:
today <- function() {
  paste0("Today is ", format(Sys.Date(), "%A"))
}


#' A line of followed by an empty line will be interpreted as title
#' 
#' content of description 
#' 
#' content for details convert its argument to a Delta14C representation
#' This function returns an object of the same type as its imput
#' this can be a number a matrix or an object of class FcAtm
#' @param x1  A number
#' @return  The square of the argument 
#' @export
simpleFunc <- function( x1 ) {  x1^2}


#' Hidden Generic
#' 
#' convert its argument to a Delta14C representation
#' This function returns an object of the same type as its imput
#' this can be a number a matrix or an object of class FcAtm
#' So it can be converted into the AbsoluteFractionModern format if a conversion is implemented.
#' @param object Should have a description
#' @keywords internal 
#' @return an object that contains data and a formatdescription.  
#' @examples
#' hiddenGeneric(2)
setGeneric(
    name="hiddenGeneric",
    def=function( object ){ standardGeneric("hiddenGeneric") }
)


g<-function( object , somethingElse ){ standardGeneric("exposedGeneric") }

#' This is the title 
#' 
#' This is the first line of the description
#' which can go on for several more
#' So it can be converted into the AbsoluteFractionModern format if a conversion is implemented.
#' @return  an object that contains data and a formatdescription.  
setGeneric(
    name="exposedGeneric",
    def=g
)

#' Exposed Class 
#' 
#' Since this class is exported in the Namespace file you can inherit from it
#' but nethertheless the method for "hiddenGeneric" with this class as
#' a signature will not be visible
#' @slot times numeric vector
#' @export
setClass(
   Class="ExposedClass",
   slots=c(times="numeric")
)

#' the next method should not appear in the help 
#' 
#' because the generic function is not exported
#' @keywords internal 
setMethod(
   f= "hiddenGeneric",
   signature="ExposedClass",
   definition=function(object){
     return(object@times)
   }
)

#' Exposed Class Method
#' 
#' This method should appear in the help 
#' because the generic function is exported
#' short description
#' here come a few details
#' in two lines
#' @examples 
#' eci <- new(Class="ExposedClass",times=1:4)
#' exposedGeneric(eci,3)
setMethod(
   f= "exposedGeneric",
   signature=c("ExposedClass","numeric"),
   definition=function#short title
   (
     object ##<< an object 
     ,
     somethingElse ##<< an object 
   ){
       return(object@times)
       ### the result
     }
)


#' This method should appear in the help 
#' because the generic function is exported
setMethod(
   f= "exposedGeneric",
   signature=c("character","numeric"),
   definition=function ( object , somethingElse ){ return(object@times) }
)

#' [ 
#'
#' (and others [[,$...) 
#' documentation is required although the method is not explicitly exported
#' We fake the behavior of the [] operator to provide a more
#' R like interface.
#' the soil model
#' the name of the method
#' details<<
#' here come the details
#' anotherFunnySection<<
#' whatever you want
#' the result of the get[[PropertyName]] method
setMethod(
  f="[",
  signature(x="ExposedClass",i="character",j="missing",drop="missing"),
  definition=function( x, i ){
    2 
  }
)

