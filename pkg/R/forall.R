
forall <- function
### For each object in the package that satisfies the criterion
### checked by subfun, parse source using FUN and return the resulting
### documentation list.
(FUN,
### Function to apply to each element in the package.
 subfun=function(x)TRUE
### Function to select subsets of elements of the package, such as
### is.function. subfun(x)==TRUE means FUN will be applied to x and
### the result will be returned.
 ){
  FUN <- FUN
  f <- function(objs,docs,...){
    if(length(objs)==0)return(list())
    objs <- objs[sapply(objs,subfun)]
    L <- list()
    on.exit(cat(sprintf("Parser Function failed on %s\n",N)))
    for(N in union(names(docs),names(objs))){
      o <- objs[[N]]
      L[[N]] <- FUN(src=getSource(o),
                    name=N,objs=objs,o=o,docs=docs,doc=docs[[N]],...)
    }
    on.exit()## remove warning message
    L
  }
  class(f) <- c("allfun","function")
  f
### A Parser Function.
}
