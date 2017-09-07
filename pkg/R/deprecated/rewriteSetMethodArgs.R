
 rewriteSetMethodArgs<-function(lang){
   ### Since we do not know if the arguments in the call to setMethod are given with
   ### keywords, partially matching keywords as an ordered list or any 
   ### combination of it, we use the same function as R  (match.arg ) 
   ### to rewrite our argumentlist to a (pair)list from which
   ### we can extract the information easily
   KeyWords=c("f","signature","definition","where")
   NamedArgs=list() # the new argument list
   args=lang[2:length(lang)]
   argNames=names(args)
   if(is.null(argNames)){ 
     # in the  special case keyword=value pairs are not given at all
     # we determine them by position
     for (i in seq_along(args)){
        NamedArgs[[KeyWords[[i]] ]] <- args[[i]]
     }
   }else{
     # at least some keyword=value pairs are given 
     # we determine them by match arg or by position
     for (i in seq_along(args)){
        argName=argNames[[i]]
        if(argNames[[i]]==""){ # no keyword=value given for this arg 
          NamedArgs[[KeyWords[[i]]]] <- args[[i]] #determining the keyword  by position
        }else{
         newName=try(match.arg(argNames[[i]],KeyWords))
         if (class(newName)=="try-error") {
           stop(paste("could not match the argument with name : " ,argNames[[i]]," to a formal argument of setMethod",sep=""))
         }else{
          NamedArgs[[newName]] <- args[[i]]
        }
       }
     }
   }
   #NN <- names(NamedArgs)
   NamedArgs
 }
