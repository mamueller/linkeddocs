
kill.prefix.whitespace <- function
### Figure out what the whitespace preceding the example code is, and
### then delete that from every line.
(ex
### character vector of example code lines.
 ){
  tlines <- gsub("\\s*","",ex)
  ##tlines <- gsub("#.*","",tlines)
  prefixes <- unique(gsub("\\S.*","",ex[tlines!=""]))
  FIND <- prefixes[which.min(nchar(prefixes))]
  ## Eliminate leading tabulations or 2/4 spaces
  sub(FIND, "", ex)
### Character vector of code lines with preceding whitespace removed.
}
