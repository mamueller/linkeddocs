
############################################################
methodTable <- function(exprs,e){
  gens=list() ## a list of generic functions that are mentioned in setMethod statements within the code to be documented
  for ( k in 1:length(exprs)){
    lang <- exprs[[k]]
    chars <- as.character(lang)
    expr.type <- chars[[1]]
    if (expr.type == "setMethod"){
      NamedArgs=rewriteSetMethodArgs(lang)
      nameOfGeneric<-NamedArgs[["f"]] 
      methSig <- eval(NamedArgs[["signature"]],e)
      gens[[nameOfGeneric]] <- unique(c(gens[[nameOfGeneric]],list(methSig)))
    }
  }
  gens
}
