
############################################################
exported=function
### a helper to read the NAMESPACE file, 
##fixme mm:
## should   be replaced by Rs own function
## since we implicitly are more specific w.r.t. syntax than R when it parses the file
(pattern,tD){
  # for simpler parsing we dont allow every possible 
  # export statement but assume the form
  # export(
  #  firstFunc,
  #  secondFunc
  # )
  ns=readLines(file.path(tD,"NAMESPACE"))
  if(any(grepl(pattern,ns))){
    fl=grep(pattern,ns)[[1]]
    # start search for closing ")" at the opening one and
    # use only the next ")" if there are several
    ll= grep("\\)",ns[fl:length(ns)])[[1]]+fl-1
    if (ll==fl+1){
      return(NULL)
    }else{
      trunks= unlist(lapply(ns[(fl+1):(ll-1)],removeComma))
      return(trunks)
    }
  }else{
    return(NULL)
  }
}
