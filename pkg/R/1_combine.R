
# vim:set ff=unix expandtab ts=2 sw=2:
### combine lists or character strings
combine <- function(x,y){
    UseMethod("combine")
}

### combine character strings by pasting them together
combine.character <- function(x,y)
    paste(x,y,sep="\n")

### combine lists by adding elements or adding to existing elements
combine.list <- function(x,y){
  toadd <- if(".overwrite"%in%names(y)){
    y <- y[names(y)!=".overwrite"]
    rep(TRUE,length(y))
  }else{
    !names(y)%in%names(x)
  }
  toup <- names(y)[!toadd]
  x[names(y)[toadd]] <- y[toadd]
  for(up in toup)x[[up]] <- combine(x[[up]],y[[up]])
  x
### A list, same type as x, but with added elements from y.
}
