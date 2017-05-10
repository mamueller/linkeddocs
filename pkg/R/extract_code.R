
# vim:set ff=unix expandtab ts=2 sw=2:
extract_code<-function(desc){
  ## concatenate code files and parse them
  # PhG: in Writing R Extensions manuals, source code in /R subdirectory can
  # have .R, .S, .q, .r, or .s extension. However, it makes sense to restrict
  # this to .R only for inlinedocs, but a clear indication is required in the
  # man page!
  code_files <- if(!"Collate"%in%colnames(desc))Sys.glob("*.R")
  else strsplit(gsub("\\s+"," ",desc[,"Collate"]),split=" ")[[1]]
  #code_files =grep(excludePattern,code_files,invert=TRUE,value=TRUE)
  ## TDH 28 Jan 2013, warn users such as Pierre Neuvial if they have
  ## comments on the last line of one input file. Sometimes comments
  # on the last line can appear to be the first line of comments of
  ## the next code file.
  lines.list <- lapply(code_files,readLines)
  for(i in seq_along(lines.list)){
    lvec <- lines.list[[i]]
    fn <- code_files[i]
    last <- lvec[length(lvec)]
    if(grepl("#",last)){
      warning("comment on last line of ",fn,
              ", unexpected docs may be extracted")
    }
  }

#  # PhG: one must consider a potential Encoding field in DESCRIPTION file!
  # which is used also for .R files according to Writing R Extensions
  if ("Encoding" %in% colnames(desc)) {
    oEnc <- options(encoding = desc[1, "Encoding"])$encoding
    on.exit(options(encoding = oEnc), add = TRUE)
  }
  code <- do.call(c,lapply(code_files,readLines))
  return(code)
}

