
############################################################
removeComma <- function(str){
  if(grepl(",",str)){
     str <- strsplit(str,",")[[1]][[1]]
  }
  return(str)
}
