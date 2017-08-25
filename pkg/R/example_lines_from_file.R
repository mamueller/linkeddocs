
# vim:set ff=unix expandtab ts=2 sw=2:
example_lines_from_file <- function(
 ref ## a line containing the path and the name of the wrapper function 
 ){
 #the file has to be sourced and the function extracted
 #fake it
 relPath <- file.path('inst','examples','examples_1.R')
 path <- file.path('pkg','inst','examples','examples_1.R')
 pe(quote(getwd()))
 # we now source the file and search for the functions that are added
 # to the current environment
 b <- environment()
 a  <- new.env(parent = b)
 source(path,a,keep.source = TRUE)
 nN <- setdiff(names(a),names(b))
 nO <- as.list(a)[nN]
 nF <- nO[sapply(nO,is.function)]
 
 lines <- '# examples from external files'
 for(name in names(nF)){ 
  lstr <- paste('#',' ',as.character(relPath),' ',name,':',collapse='',sep='')
  fstr <- extract_function_body_with_comments(nF[[name]])
  lines <- c(lines,lstr,fstr)
 }
 pp('lines')
 return(lines)
}
