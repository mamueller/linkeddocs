## vim:set ff=unix expandtab ts=2 sw=2:
## title from first line of function def
title.from.firstline <- function
### extract the title from the first line of a function definition
(src,...){
  first <- src[1]
  if(!is.character(first))return(list())
  if(!grepl("#",first))return(list())
  # fixme:mm 
  # It would be enough to return the result as a string and not as a 
  # list which is done for backward compatibility at the moment.
  list(title=gsub("[^#]*#\\s*(.*)","\\1",first,perl=TRUE))
}
