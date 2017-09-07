
############################################################
setMethod("[",
    signature(x = "listOfMethods", i = "logical"),
    function 
    ### overload the [] operator for objects of class "listOfMethods"
    (x, i, j, ..., drop = TRUE) 
    {
       fdef <- x@generic
       object <- new("listOfMethods", arguments = fdef@signature)
       object@generic <- fdef
       object@signatures  <- x@signatures[i]
       object@.Data       <-      x@.Data[i]
       object@names       <-      x@names[i]
       object
       
    }
)
