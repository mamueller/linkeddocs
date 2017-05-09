#
# vim:set ff=unix expandtab ts=2 sw=2:
writeFlattenedListToRd<-function(flat,fn){

  for (section in names(flat)){
    content<-paste0(flat[[section]],collapse="\n")
    flat[[section]]=sprintf("\\%s{%s}",section,content)
  }
  cat(mask_special_Rd_characters(unlist(flat)), file = fn, sep = "\n")
}
