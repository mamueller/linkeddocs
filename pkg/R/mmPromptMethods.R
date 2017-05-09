
# vim:set ff=unix expandtab ts=2 sw=2:
############################################################
mmPromptMethods <-  function (genName, filename = NULL, exportedMeths,where,nsi) 
  ## this is a copy of R s own promptMehtods functions but
  ## with an additional argument of the methods to be exported (and documented)
{
    gens<-nsi[["gens"]]
    pe(quote(names(gens)),environment())
    dms<-nsi[["documentableMeths"]]

    genExported  <- !is.null(exportedMeths)

    escape <- function(txt) gsub("%", "\\\\%", txt)
    packageString <- ""
    #fdef <- getGeneric(genName,where=where)
    #if (!isGeneric(f=genName ,where=where,fdef = fdef)) 
    #    stop(gettextf("no generic function found corresponding to %s", 
    #        sQuote(genName)), domain = NA)
    #methods <- findMethods(fdef,where=where)
    fdef <-gens[[genName]] 
    methods<-dms[[genName]]
    
    #where <- .genEnv(fdef, topenv(parent.frame()))
    #if (!identical(where, .GlobalEnv)) 
    #    packageString <- sprintf("in Package \\pkg{%s}", 
    #        getPackageName(where))
    fullName <- utils:::topicName("methods", genName)

    n <- length(methods)
    labels <- character(n)
    aliases <- character(n)
    signatures <- findMethodSignatures(methods = methods, target = TRUE)
    args <- colnames(signatures)
    for (i in seq_len(n)) {
        sigi <- signatures[i, ]
        #sigiN <- matchSignature(sigi,fdef,where) # normalize the signature 
        sigiN <- matchSignature(sigi,fdef) # normalize the signature 
        labels[[i]] <- sprintf("\\code{signature(%s)}", paste(sprintf("%s = \"%s\"", 
            args, escape(sigi)), collapse = ", "))
      # mm:
      # Since the methods have their own file we do not want the aliases
      # here but rather links to the actual method files
      #  aliases[[i]] <- paste0("\\alias{", utils:::topicName("method", 
      #      c(genName, signatures[i, ])), "}")
    }
    ####
    if(genExported){
      exportedSignatures <-findMethodSignatures(methods =exportedMeths, target = TRUE)
      n=nrow(exportedSignatures)
      labels <- character(n)
      items<- character(n)
      args <- colnames(exportedSignatures)
      for (i in seq_len(n)) {
        sigi <- exportedSignatures[i, ]
        sigiN <- matchSignature(sigi,fdef,where) # normalize the signature 
        #N <- methodDocName(genName,sigiN)
        N <- methodDocName(genName,sigi)
        labels[[i]] <- sprintf("\\code{signature(%s)}", paste(sprintf("%s = \"%s\"", 
            args, escape(sigiN)), collapse = ", "))
        items[[i]]<- paste0("    \\item{", labels[[i]], "}{\n      \\code{\\link{",N,"}}  \n    }")

      }
      des <- paste0(
        "\\description{\n ~~ Methods for function",
        " \\code{", genName, "}", 
        sub("^in Package", "in package", packageString),
        " ~~\n}"
      )
      
      text <- c("\\section{Methods}{\n  \\describe{", items, "\n  }\n}")

    }else{
      des <- paste0(
        "\\description{\n All methods for function",
        " \\code{", genName, "} ", 
        "are intended for internal use inside the package only. \n}"
      )
      #item<-'
      #All methods for this generic are privat. (not exported into the namespace).
      #To discourage use outside the package the documentation is truncated.
      #'
      #text <- c("\\section{Methods}{\n\\describe{", item, "}\n}")
      text <- "" #no method section at all
    }
    aliasText <- c(paste0("\\alias{", escape(fullName), "}"), 
        escape(aliases))
    if (identical(filename, FALSE)) 
        return(c(aliasText, text))
    if (is.null(filename) || identical(filename, TRUE)) 
        filename <- paste0(fullName, ".Rd")
    Rdtxt <- list(name = paste0("\\name{", fullName, "}"), type = "\\docType{methods}", 
        aliases = aliasText, title = sprintf("\\title{ ~~ Methods for Function \\code{%s} %s ~~}", 
            genName, packageString), description = des 
        , `section{Methods}` = text, 
        keywords = c("\\keyword{methods}", "\\keyword{ ~~ other possible keyword(s) ~~ }"))
    if (is.na(filename)) 
        return(Rdtxt)
    cat(unlist(Rdtxt), file = filename, sep = "\n")
#print(paste("A shell of methods documentation has been written",filename))
    invisible(filename)
}

