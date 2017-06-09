#
# vim:set ff=unix expandtab ts=2 sw=2:
##########################################################################
extract.docs.file <- structure(function
### Apply all parsers relevant to extract info from just 1 code file.
### a wrapper for \code\link{extract.docs.file}}
(f,
### File name of R code to read and parse.
 parsers=NULL,
### Parser Functions to use to parse the code and extract
### documentation.
inlinedocs.exampleDir=file.path("..","..","inst","tests"),
### A string pointing to the location where inlinedocs should search for external examples
inlinedocs.exampleTrunk="example.",
### A string used to identify the files containing external examples in the example directory. All file names of external examples have to start with this string
 ...
### Other arguments to pass to Parser Functions.
 ){
  extract.docs.lines(readLines(f),parsers,inlinedocs.exampleDir,inlinedocs.exampleTrunk)
},ex=function(){
  f <- system.file("silly","R","silly.R",package="inlinedocs")
  extract.docs.file(f)
})

