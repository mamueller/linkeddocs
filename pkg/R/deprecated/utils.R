### Copied from R-3.0.1, to support getKnownS3generics.
getKnownS3generics <- function(){
  c(names(.knownS3Generics), get_internal_S3_generics())
}

### Copied from R-3.0.1, to support getKnownS3generics.
get_internal_S3_generics <- function(primitive = TRUE){
  out <- c("[", "[[", "$", "[<-", "[[<-", "$<-", "as.vector", 
           "unlist", get_S3_primitive_generics())
  if (!primitive) 
    out <- out[!vapply(out, is_primitive_in_base, NA)]
  out
}

### Copied from R-3.0.1, to support getKnownS3generics.
is_primitive_in_base <- function(fname){
  is.primitive(get(fname, envir = baseenv(), inherits = FALSE))
}

### Copied from R-3.0.1, to support getKnownS3generics.
get_S3_primitive_generics <- function(include_group_generics = TRUE){
  if (include_group_generics) 
    c(base::.S3PrimitiveGenerics, "abs", "sign", "sqrt", 
      "floor", "ceiling", "trunc", "round", "signif", "exp", 
      "log", "expm1", "log1p", "cos", "sin", "tan", "acos", 
      "asin", "atan", "cosh", "sinh", "tanh", "acosh", 
      "asinh", "atanh", "lgamma", "gamma", "digamma", "trigamma", 
      "cumsum", "cumprod", "cummax", "cummin", "+", "-", 
      "*", "/", "^", "%%", "%/%", "&", "|", "!", "==", 
      "!=", "<", "<=", ">=", ">", "all", "any", "sum", 
      "prod", "max", "min", "range", "Arg", "Conj", "Im", 
      "Mod", "Re")
  else base::.S3PrimitiveGenerics
}

### Copied from R-3.0.1, to support findGeneric.
findGeneric <- function(fname, envir){
  if (!exists(fname, mode = "function", envir = envir)) 
    return("")
  f <- get(fname, mode = "function", envir = envir)
  if (.isMethodsDispatchOn() && methods::is(f, "genericFunction")) {
    fMethsEnv <- methods::getMethodsForDispatch(f)
    r <- lapply(grep("^ANY\\b", ls(envir = fMethsEnv), value = TRUE), 
                get, envir = fMethsEnv)
    if (any(ddm <- unlist(lapply(r, class)) == "derivedDefaultMethod")) 
      f <- r[ddm][[1]]@.Data
    else warning(gettextf("'%s' is a formal generic function; S3 methods will not likely be found", 
                          fname), domain = NA)
  }
  isUMEbrace <- function(e) {
    for (ee in as.list(e[-1L])) if (nzchar(res <- isUME(ee))) 
      return(res)
    ""
  }
  isUMEif <- function(e) {
    if (length(e) == 3L) 
      isUME(e[[3L]])
    else {
      if (nzchar(res <- isUME(e[[3L]]))) 
        res
      else if (nzchar(res <- isUME(e[[4L]]))) 
        res
      else ""
    }
  }
  isUME <- function(e) {
    if (is.call(e) && (is.name(e[[1L]]) || is.character(e[[1L]]))) {
      switch(as.character(e[[1L]]), UseMethod = as.character(e[[2L]]), 
             `{` = isUMEbrace(e), `if` = isUMEif(e), "")
    }
    else ""
  }
  isUME(body(f))
}


### Copied from R-3.2.2, and changed to write the PackageRd File 
mm_promptPackage= function (package, filename,lib.loc = NULL, description) 
{
    insert1 <- function(field, new) {
        prev <- Rdtxt[[field]]
        Rdtxt[[field]] <<- c(prev[-length(prev)], new, prev[length(prev)])
    }
    insert2 <- function(field, new) insert1(field, paste("~~", 
        new, "~~"))
    name <- paste0(package, "-package")
    Rdtxt <- list(name = paste0("\\name{", name, "}"), aliases = c(paste0("\\alias{", 
        name, "}"), c(paste0("\\alias{", package, "}"))), docType = "\\docType{package}", 
        title = c("\\title{", "}"), description = c("\\description{", 
            "}"), details = c("\\details{", "}"), author = c("\\author{", 
            "}"), references = character(0L), keywords = c("\\keyword{ package }"))
    insert1("title", paste0("\\packageTitle{", package, "}"))
    insert1("description", paste0("\\packageDescription{", package, 
        "}"))
    insert1("author", c(paste0("\\packageAuthor{", package, "}"), 
        "", paste("Maintainer:", paste0("\\packageMaintainer{", 
            package, "}"))))
    insert1("details", c("", "The DESCRIPTION file:"))
    insert1("details", paste0("\\packageDESCRIPTION{", package, 
        "}"))
    insert1("details", paste0("\\packageIndices{", package, "}"))
    #insert2("details", strwrap("An overview of how to use the package, including the most important functions"))
    #Rdtxt$references <- c("\\references{", paste("~~", "Literature or other references for background information", 
    #    "~~"), "}")
    #Rdtxt$seealso <- c("\\seealso{", "}")
    #insert2("seealso", c("Optional links to other man pages, e.g.", 
    #    "\\code{\\link[<pkg>:<pkg>-package]{<pkg>}}"))
    #Rdtxt$examples <- c("\\examples{", "}")
    #insert2("examples", "simple examples of the most important functions")
    #insert2("keywords", strwrap("Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory"))
    cat(unlist(Rdtxt), file = filename, sep = "\n")
    message(gettextf("Created file named %s.", sQuote(filename)), 
        "\n", gettext("Edit the file and move it to the appropriate directory."), 
        domain = NA)
    #invisible(filename)
}

