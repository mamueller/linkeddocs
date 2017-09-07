
extract.docs.file <- function
### Apply all parsers relevant to extract info from just 1 code file.
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
  if(is.null(parsers))parsers <- nondesc.parsers
  apply.parsers(
	readLines(f),
	parsers,
	verbose=FALSE,
	inlinedocs.exampleDir,
	inlinedocs.exampleTrunk,
	...
	)[["docs"]]
}
