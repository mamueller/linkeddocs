extract.docs<-function
### produce doc link instances
(parsed,objs,on){
  extract.docs.try <-function(o,on)
    {
      ## Note: we could use parsed information here too, but that
      ## would produce different results for R.methodsS3::setMethodS3 etc.
      doc <- list()
      if ( !is.null(parsed[[on]]) ){
        if ( !is.na(parsed[[on]]@code[1]) ){ # no code given for generics
          doc$definition <- paste(parsed[[on]]@code)
        }
        if(!"description"%in%names(doc) && !is.na(parsed[[on]]@description) ){
          doc$description <- parsed[[on]]@description
        }
        ## if ( "R.methodsS3::setMethodS3" == parsed[[on]]@created ){
        ##   gen <- leadingS3generic(on,topenv())
        ##   if ( 0 < length(gen) ){
        ##     doc$.s3method <- gen$.s3method
        ##     cat("S3method(",gen$.s3method[1],",",gen$.s3method[2],")\n",sep="")
        ##   }
        ## }
      }
      if("title" %in% names(doc) && !"description" %in% names(doc) ){
        ## For short functions having both would duplicate, but a
        ## description is required. Therefore automatically copy title
        ## across to avoid errors at package build time.
        doc$description <- doc$title
      }
      doc
    }
    res <- try({o <- objs[[on]]
                extract.docs.try(o, on)},FALSE)
    if(class(res)=="try-error"){
      cat("Failed to extract docs for: ",on,"\n\n")
      list()
    } else if(0 == length(res) && inherits(objs[[on]],"standardGeneric")){
      NULL
    } else if(0 == length(res) && "function" %in% class(o)
              && 1 == length(osource <- getSource(o))
              && grepl(paste("UseMethod(",on,")",sep="\""),osource)
              ){
      ## phew - this should only pick up R.oo S3 generic definitions like:
      ## attr(*, "source")= chr "function(...) UseMethod(\"select\")"
      NULL
    } else res
  }
