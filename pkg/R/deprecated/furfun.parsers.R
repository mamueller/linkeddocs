
### Parsers for each function that are constructed automatically. This
### is a named list, and each element is a parser function for an
### individual object.
forfun.parsers <-
  list(
       #prefixed.lines=prefixed.lines,
       extract.xxx.chunks=extract.xxx.chunks,
       title.from.firstline=title.from.firstline,
       ## PhG: it is tests/FUN.R!!! I would like more flexibility here
       ## please, let me choose which dir to use for examples!
       ## Get examples for FUN from the file tests/FUN.R
       examples.from.testfile=function(name,...){
         tsubdir <- getOption("inlinedocs.exdir")
         if (is.null(tsubdir)) tsubdir <- "tests"	# Default value
         tfile <- file.path("..",tsubdir,paste(name,".R",sep=""))
#print(file.exists(tfile))
         if(file.exists(tfile)){
           list(examples=readLines(tfile))
         }
         else list()
       },
       mm.examples.from.testfile=mm.examples.from.testfile,
       definition.from.source=definition.from.source
       )
