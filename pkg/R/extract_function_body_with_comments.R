
# vim:set ff=unix expandtab ts=2 sw=2:
extract_function_body_with_comments<- function(func){

 ### base::body unfortunately throughs away the comments
 srcRef <- utils::getSrcref(func)
 codeText <- as.character(srcRef,useSource=T)
 # remove the wrapping code
 require(stringr)
 l <- length(codeText)
 codeText[[1]] <- str_replace(codeText[[1]],'.*function\\(.*\\)\\{','')

 codeText[[l]] <- str_replace(codeText[[l]],'\\}','')
 return(as.character(lapply(codeText,str_trim)))
}
