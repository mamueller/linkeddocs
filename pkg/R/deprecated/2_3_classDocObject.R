#
# vim:set ff=unix expandtab ts=2 sw=2:
classDocObject<-setClass(Class="classDocObject",contains="DocLink",slots=c(l="list",nsi="list",env="environment"))
#-------------------------------------------------------------------------
setMethod(
  f="write_Rd_file",
  signature=signature(obj="classDocObject",fn="character"),
  def=function(
      obj,
      fn
    ){
    d <- obj@l
    ## the list d is nested e.g. for arguments
    ## we now flatten it so that it only has a 
    ## character vector for each section
    flat<-list()
    on <- paste(obj@name,"class",sep="-")
    flat[["name"]] <-on
    flat[["alias"]] <- on
    #mm:fake
    flat[["docType"]] <- "class"
    flat[["description"]] <- toString(paste(obj@description,sep="\n"))
    flat[["section{Methods}"]] <- Rd_method_lines(obj)
    
    chunk.source <- obj@code
    title<- lonely$title.from.firstline(chunk.source)
  	if ( 0 == length(title) ){
  	  title <- list(title=paste(obj@name,"S4 class"))
  	}
    flat[["title"]]<-title
   # pp("title")
    #stop("###################### mm ####################")
    # add the parts from d that could be extracted 
    #target_secs<-c("title","description","references","note","value")
    target_secs<-c("title","description","section{Methods}")
    for (sec in target_secs){
      if (is.element(sec,names(d))){
        flat[[sec]]<-d[[sec]]
      }
    } 
    writeFlattenedListToRd(flat,fn)
  }
)

#-------------------------------------------------------------------------
setMethod(
  f="Rd_method_lines",
  signature=signature(obj="classDocObject"),
  def=function(
      obj
      ){
        nsi<-obj@nsi

        whereClass <-obj@env
        clName<-obj@name
        where <- obj@env

        # we only link to methods we can see when the package is loaded with library
        # this might include methods for generics that we didn't define like [, [[ $ 
        # and the like but excludes methods for generics that we did not export
        # which is what we want here
        methnms <- intersect(genWithClass(clName,where ),nsi[["gens_visible_in_pkg"]])
        nmeths=length(methnms)

        if (nmeths > 0) {
          .meths.body <- "  \\describe{"
          for (i in 1L:nmeths) {
              
              .sig <- sigsList(methnms[i], where = whereClass)
             # pp(".sig")
              for (j in seq_along(.sig)) {
                  # find signatures containing the class we are documenting
                  msigs=match(.sig[[j]], clName)
                  if (!all(is.na(msigs))) {
                     methn.i <- escape(methnms[i])
                    # the signature list might still contain several ANY 
                    # arguments and 
                    # 
                    #mm: we add a \link to the methods here
                    cur <- paste(.sig[[j]], collapse = ",")
                    target_alias=methodDocName(methn.i,.sig[[j]])
                    .meths.body <- c(.meths.body, paste0("    \\item{", 
                      methn.i, "}{\\code{signature", pastePar(.sig[[j]]), 
                      "}: ... }"," \\code{\\link{",target_alias,"}}"))
                    #.methAliases <- paste0(.methAliases, "\\alias{", 
                    #  methn.i, ",", cur, "-method}\n")
                  }
              }
          }
          .meths.body <- c(.meths.body, "\t }")
        }else{
        .meths.body <- paste("No methods defined with class", 
            obj@name, "in the signature.")
        }
          
   }
)
