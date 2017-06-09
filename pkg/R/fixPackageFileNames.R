
# vim:set ff=unix expandtab ts=2 sw=2:
### Copied from R-3.0.1, to support fixPackageFileNames.
fixPackageFileNames <- function(list){
  list <- as.character(list)
  if (length(list) == 0L) 
    return(list)
  list0 <- gsub("[[:cntrl:]\"*/:<>?\\|]", "_", list)
  #list0 <- gsub("#", "_", list0) #mm this was necessary to get rid of th "#" occuring in method signature strings as separator  
  wrong <- grep("^(con|prn|aux|clock\\$|nul|lpt[1-3]|com[1-4])(\\..*|)$", 
                list0)
  if (length(wrong)) 
    list0[wrong] <- paste0("zz", list0[wrong])
  ok <- grepl("^[[:alnum:]]", list0)
  if (any(!ok)) 
    list0[!ok] <- paste0("z", list0[!ok])
  list1 <- tolower(list0)
  list2 <- make.unique(list1, sep = "_")
  changed <- (list2 != list1)
  list0[changed] <- list2[changed]
  list0
}
