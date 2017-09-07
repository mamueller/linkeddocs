
### List of parsers to use by default with package.skeleton.dx.
default.parsers <-
  c(
    #extra.class.docs=extra.class.docs, ## TODO: cleanup!
    #extra.method.docs=extra.method.docs, ## TODO: cleanup!
    #extra.code.docs=extra.code.docs, ## TODO: cleanup!
    sapply(forfun.parsers,forfun),
    edit.package.file=function(desc,...){
      in.details <- setdiff(colnames(desc),"Description")
      details <- sprintf("%s: \\tab %s\\cr",in.details,desc[,in.details])
      L <-
        list(list(title=desc[,"Title"],
                  description=desc[,"Description"],
                  `tabular{ll}`=details))
      names(L) <- paste(desc[,"Package"],"-package",sep="")
      L
    },
    sapply(forall.parsers,forall)
    )
