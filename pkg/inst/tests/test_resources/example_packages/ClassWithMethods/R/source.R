#
# vim:set ff=unix expandtab ts=2 sw=2:
#------------------------------------------------
setGeneric(
    name="hiddenGeneric",
    def=function( # convert its argument to a Delta14C representation
    ### Thfunction returns an object of the same type as its imput
    ### this can be a number a matrix or an object of class FcAtm
    object ##<< an object that contains data and a formatdescription.  So it can be converted into the AbsoluteFractionModern format if a conversion is implemented.
    ){
        standardGeneric("hiddenGeneric")
    }
)
#------------------------------------------------
### a preceding line of comment
g<-function( # convert its argument to a Delta14C representation
    ### This function returns an object of the same type as its imput
    ### this can be a number a matrix or an object of class FcAtm
    object ##<< an object that contains data and a formatdescription.  So it can be converted into the AbsoluteFractionModern format if a conversion is implemented.
    ){
        standardGeneric("exposedGeneric")
    }
setGeneric(
    name="exposedGeneric",
    def=g
)

