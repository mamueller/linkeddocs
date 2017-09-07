
getSource <- function
### Extract a function's source code.
(fun.obj
### A function.
 ) {
      srcref <- attr(fun.obj, "srcref")
      if (!is.null(srcref)) {
        ##unlist(strsplit(as.character(srcref), "\n"))
        as.character(srcref)
      }
      else attr(fun.obj, "source")
### Source code lines as a character vector.
}
