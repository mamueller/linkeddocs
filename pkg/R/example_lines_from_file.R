example_lines_from_file <- function(
 ref ## a line containing the path and the name of the wrapper function 
 ){
 #the file has to be sourced and the function extracted
 #fake it
 path <- file.path('pkg','inst','examples','examples_1.R')
 pe(quote(getwd()))
 # we now source the file and search for the functions that are added
 # to the current environment
 ob <-ls()
 source(path)
 oa <- ls()
 on <- setdiff(oa,ob)
 pp('on')
 lines <- as.character('

   # examples from external files

   # inst/examples/example1.R func1: 
   eci <- new(Class="ExposedClass",1:4)
   exposedGeneric(eci,1)

   # inst/examples/example1.R func2: 
   eci <- new(Class="ExposedClass",1:4)
   exposedGeneric(eci,2)
 ')
 return(lines)
}
