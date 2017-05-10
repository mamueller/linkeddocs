
# vim:set ff=unix expandtab ts=2 sw=2:
extract_description<-function(descfile){
  ## Default values and required fields in DESCRIPTION file.
  description.defaults <-
    c("Package"="",
      "Maintainer"=Sys.getenv("USER"),
      "Author"=Sys.getenv("USER"),
      "Version"="1.0",
      "License"="GPL-3",
      "Title"="a package",
      "Description"="a package that does\n many things.")

  ## Necessary fields in DESCRIPTION, otherwise error.
  fields <- names(description.defaults)

  ## Default DESCRIPTION, written if it doesn't exist.
  empty.description <-
    matrix(description.defaults,ncol=length(fields),dimnames=list(NULL,fields))


  
  
  desc <- read.dcf(descfile)
  ## TDH 3 Sept 2013 need to support Authors@R for CRAN.
  if("Authors@R" %in% colnames(desc)){
    author <- paste(eval(parse(text=desc[,"Authors@R"])), collapse=", ")
    desc <- cbind(desc,
                  Author=author,
                  Maintainer=author)
  }
  if(any(f <- !sapply(fields,is.element,colnames(desc))))
    stop("Need ", paste(names(f)[f], collapse = ", "), " in ", descfile)
    #PhG: corrected from stop("Need ",names(f)[f]," in ",descfile)
  if(any(f <- sapply(fields,function(f)desc[,f]=="")))
    stop("Need a value for ", paste(names(f)[f], collapse = ", "),
         " in ", descfile)
  return(desc)
}
