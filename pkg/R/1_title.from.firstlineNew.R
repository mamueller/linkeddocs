## vim:set ff=unix expandtab ts=2 sw=2:
## title from first line of function def
title.from.firstlineNew <- function
### extract the title from the first line of a function definition
(src,...){
  first <- src[1]
  if(!grepl("#",first))return(NULL)
  return(gsub("[^#]*#\\s*(.*)","\\1",first,perl=TRUE))
}
