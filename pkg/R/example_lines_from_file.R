
# vim:set ff=unix expandtab ts=2 sw=2:
example_lines_from_file <- function(
 ref, ## a line containing the path and the name of the wrapper function 
 pkgDir='pkg'
 ){
 #the file has to be sourced and the function extracted
 relPathStr <- unlist(str_split(ref,'\\s+'))[[1]]
 name <-  unlist(str_split(ref,'\\s+'))[[2]]
 parts <- unlist(str_split(relPathStr,'/'))
 relPath <- paste(parts,collapse=.Platform$file.sep) # for some reason file.path does not work with the output of str_split
 path <- file.path(pkgDir,relPath)
 pp('path')
 pe(quote(getwd()))
 # we now source the file and search for the functions that are added
 # to the current environment
 b <- environment()
 a  <- new.env(parent = b)
 source(path,a,keep.source = TRUE)
 nN <- setdiff(names(a),names(b))
 nO <- as.list(a)[nN]
 nF <- nO[sapply(nO,is.function)]
 
 if(is.element(name,names(nF))){ 
  lstr <- paste('#',' ',as.character(relPath),' ',name,':',collapse='',sep='')
  fstr <- extract_function_body_with_comments(nF[[name]])
  lines <- c(lstr,fstr)
  return(lines)
 }else{
  stop(paste('the example function:',name,' could not be found in the file:',path,sep=""))
  }
}
