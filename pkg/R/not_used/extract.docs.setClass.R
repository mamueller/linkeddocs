#
# vim:set ff=unix expandtab ts=2 sw=2:
extract.docs.setClass <- function # S4 class inline documentation
### Using the same conventions as for functions, definitions of S4 classes
### in the form \code{setClass("classname",\dots)} are also located and
### scanned for inline comments.
(doc.link
### DocLink object as created by \code{extract.file.parse}.
### Note that \code{source} statements are \emph{ignored} when scanning for
### class definitions.
 ){
  chunk.source <- doc.link@code
  ##details<<
  ## Extraction of S4 class documentation is currently limited to expressions
  ## within the source code which have first line starting with
  ## \code{setClass("classname"}. These are located from the source file
  ## (allowing also for white space around the \code{setClass} and \code{(}).
  ## Note that \code{"classname"} must be a quoted character string;
  ## expressions returning such a string are not matched.
  class.name <- doc.link@name

  ##details<< For class definitions, the slots (elements of the
  ## \code{representation} list) fill the role of function
  ## arguments, so may be documented by \code{##<<} comments on
  ## the same line or \code{### } comments at the beginning of the
  ## following line.
  f.n <- paste(class.name,"class",sep="-")
  docs <- extract.xxx.chunks(chunk.source,f.n)
  ## also apply source parsing functions that I separated out into
  ## separate functions
  docs <- combine(docs,lonely$prefixed.lines(chunk.source))
  docs$title <- lonely$title.from.firstline(chunk.source)
  ##details<<
  ## If there is no explicit title on the first line of setClass, then
  ## one is made up from the class name.
  if ( 0 == length(docs$title) ){
    docs$title <- list(title=paste(class.name,"S4 class"))
  }
  ##details<<
  ## The class definition skeleton includes an \code{Objects from the Class}
  ## section, to which any \code{##details<<} documentation chunks are
  ## written. It is given a vanilla content if there are no specific
  ## \code{##details<<} documentation chunks.
  if ( is.null(docs[["details"]]) ){
    docs[["details"]] <-
      paste("Objects can be created by calls of the form \\code{new(",
            class.name," ...)}",sep="")
  }
  docs[["section{Objects from the Class}"]] <- docs[["details"]]
  ## seealso has a skeleton line not marked by ~ .. ~, so have to suppress
  if ( is.null(docs[["seealso"]]) ){
    docs[["seealso"]] <- ""
  }
  if ( is.null(docs[["alias"]]) ){
    docs[["alias"]] <- paste(class.name,"-class",sep="")
  }
  if ( is.null(docs[["description"]]) ){
    docs[["description"]] <- toString(paste(doc.link@description,sep="\n"))
  }
  invisible(docs)
}

