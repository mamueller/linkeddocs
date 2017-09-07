
### List of Parser Functions that can be applied to any object.
forall.parsers <-
  list(## Fill in author from DESCRIPTION and titles.
       author.from.description=function(desc,...){
         list(author=desc[,"Author"])
       },
       ## The format section sometimes causes problems, so erase it.
       erase.format=function(...){
         list(format="")
       },
       ## Convert the function name to a title.
       title.from.name=function(name,doc,...){
         if("title"%in%names(doc))list() else
         list(title=gsub("[._]"," ",name))
       },
       ## PhG: here is what I propose for examples code in the 'ex' attribute
       examples.in.attr =  function (name, o, ...) {
         ex <- attr(o, "ex",exact=TRUE)
         if (!is.null(ex)) {
           ## Special case for code contained in a function
           if (inherits(ex, "function")) {
             ## If source is available, start from there
             src <- getSource(ex)
             if (!is.null(src)) {
               ex <- src
             } else { ## Use the body of the function
               ex <- deparse(body(ex))
             }
             ## Eliminate leading and trailing code
             ex <- ex[-c(1, length(ex))]
             if( length(ex) ){  # avoid error on yet empty example
                 if(ex[1]=="{")ex <- ex[-1]
                 ## all the prefixes
                 #mm:
                 #commented out the foloowing line 
                 #ex <- kill.prefix.whitespace(ex)
             }
             ## Add an empty line before and after example
             ex <- c("", ex, "")
           }
           list(examples = ex)
         } else list()
       },collapse=function(doc,...){
         L <- lapply(doc,paste,collapse="\n")
         L$.overwrite <- TRUE
         L
       },tag.s3methods=leadingS3generic
       )
