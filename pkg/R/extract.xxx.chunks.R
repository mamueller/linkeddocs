
## vim:set ff=unix expandtab ts=2 sw=2:
extract.xxx.chunks <- function # Extract documentation from a function
### Given source code of a function, return a list describing inline
### documentation in that source code.
(src,
### The source lines of the function to examine, as a character
### vector.
 name.fun="(unnamed function)",
### The name of the function/chunk to use in warning messages.
 ...
### ignored.
 ){
  res <- list()
  ##details<< For simple functions/arguments, the argument may also be
  ## documented by appending \code{##<<} comments on the same line as the
  ## argument name. Mixing this mechanism with \code{###} comment lines for
  ## the same argument is likely to lead to confusion, as the \code{###}
  ## lines are processed first.
  #arg.pat <- paste("^[^=,#]*?([\\w\\.]+)\\s*([=,].*|\\)\\s*)?",
  #                 "<<\\s*(\\S.*?)\\s*$",
  #                 sep="##") # paste avoids embedded trigger fooling the system
   #tw: removed first comma
   arg.pat <- paste("^[^=#]*?([\\w\\.]+)\\s*([=,].*|\\)\\s*)?",
	   "<<\\s*(\\S.*?)\\s*$",
   		sep="##") # paste avoids embedded trigger fooling the system

  skeleton.fields <- xxx.chunkNames()
  #skeleton.fields <- c("alias","details","keyword","references","author", "note","seealso","value","title","description", "describe","end","examples")
  ##details<< Additionally, consecutive sections of \code{##} comment
  ## lines beginning with \code{##}\emph{xxx}\code{<<} (where
  ## \emph{xxx} is one of the fields: \code{alias}, \code{details},
  ## \code{keyword}, \code{references}, \code{author}, \code{note},
  ## \code{seealso}, \code{value}, \code{title} or \code{description})
  ## are accumulated and inserted in the relevant part of the .Rd
  ## file.
  ##
  ## For \code{value}, \code{title}, \code{description} and function
  ## arguments, these \emph{append} to any text from "prefix"
  ## (\code{^### }) comment lines, irrespective of the order in the
  ## source.
  ##
  ## When documenting S4 classes, documentation from \code{details}
  ## sections will appear under a section \code{Objects from the Class}. That
  ## section typically includes information about construction methods
  ## as well as other description of class objects (but note that the
  ## class Slots are documented in a separate section).

  ## but this should not appear, because separated by a blank line
  extra.regexp <- paste("^\\s*##(",paste(skeleton.fields,collapse="|"),
                        ")<<\\s*(.*)$",sep="")
  cont.re <- "^\\s*##\\s*"
  in.describe <- 0
  first.describe <- FALSE
  k <- 1
  in.chunk <- FALSE
  end.chunk <- function(field,payload)
    {
      if ( "alias" == field ){
        ##note<< \code{alias} extras are automatically split at new lines.
        payload <- gsub("\\n+","\\}\n\\\\alias\\{",payload,perl=TRUE)
        chunk.sep <- "}\n\\alias{"
      } else if ( "keyword" == field ){
        ##keyword<< documentation utilities
        ##note<< \code{keyword} extras are automatically split at white space,
        ## as all the valid keywords are single words.
        payload <- gsub("\\s+","\\}\n\\\\keyword\\{",payload,perl=TRUE)
        chunk.sep <- "}\n\\keyword{"
      } else if ( "title" == field ){
        chunk.sep <- " "
      } else if ( "description" == field ){
        chunk.sep <- "\n"
      } else {
        ##details<< Each separate extra section appears as a new
        ## paragraph except that: \itemize{\item empty sections (no
        ## matter how many lines) are ignored;\item \code{alias} and
        ## \code{keyword} sections have special rules;\item
        ## \code{description} should be brief, so all such sections
        ## are concatenated as one paragraph;\item \code{title} should
        ## be one line, so any extra \code{title} sections are
        ## concatenated as a single line with spaces separating the
        ## sections.}
        chunk.sep <- "\n\n"
      }
      chunk.res <- NULL
      if ( !grepl("^\\s*$",payload,perl=TRUE) )
        chunk.res <-
          if ( is.null(res[[field]]) ) payload
          else paste(res[[field]], payload, sep=chunk.sep)
      invisible(chunk.res)
    }
  while ( k <= length(src) ){
    line <- src[k]
    ##if(grepl("^$",line))browser()
    if ( grepl(extra.regexp,line,perl=TRUE) ){
      ## we have a new extra chunk - first get field name and any payload
      new.field <- gsub(extra.regexp,"\\1",line,perl=TRUE)
      new.contents <- gsub(extra.regexp,"\\2",line,perl=TRUE)
      ##cat(new.field,"\n-----\n",new.contents,"\n\n")
      ##details<< As a special case, the construct \code{##describe<<} causes
      ## similar processing to the main function arguments to be
      ## applied in order to construct a describe block within the
      ## documentation, for example to describe the members of a
      ## list. All subsequent "same line" \code{##<<} comments go into that
      ## block until terminated by a subsequent \code{##}\emph{xxx}\code{<<} line.
      if ( "describe" == new.field ){
        ##details<< Such regions may be nested, but not in such a way
        ## that the first element in a \code{describe} is another
        ## \code{describe}.  Thus there must be at least one
        ## \code{##<<} comment between each pair of
        ## \code{##describe<<} comments.
        if ( first.describe ){
          stop("consecutive ##describe<< at line",k,"in",name.fun)
        } else {
          if ( nzchar(new.contents) ){
            if ( is.null(payload) || 0 == nzchar(payload) ){
              payload <- new.contents
            } else {
              payload <- paste(payload,new.contents,sep="\n\n")
            }
          }
          first.describe <- TRUE
        }
      } else if ( "end" == new.field ){
        ##details<< When nested \code{describe} blocks are used, a comment-only
        ## line with \code{##end<<} terminates the current level only; any
        ## other valid \code{##}\emph{xxx}\code{<<} line terminates
        ## all open describe blocks.
        if ( in.describe>0 ){
          ## terminate current \item and \describe block only
          if ( "value" == cur.field && 1 == in.describe ){
            payload <- paste(payload,"}",sep="")
          } else {
            payload <- paste(payload,"}\n}",sep="")
          }
          in.describe <- in.describe-1;
        } else {
          warning("mismatched ##end<< at line ",k," in ",name.fun)
        }
        if ( nzchar(new.contents) ){
          if ( nzchar(payload) ){
            payload <- paste(payload,new.contents,sep="\n")
          } else {
            payload <- new.contents
          }
        }
      } else {
        ## terminate all open \describe blocks (+1 because of open item)
        if ( 0 < in.describe ){
          if ( "value" != cur.field ){  # value is implicit describe block
            payload <- paste(payload,"}",sep="")
          }
          while ( in.describe>0 ){
            payload <- paste(payload,"}",sep="\n")
            in.describe <- in.describe-1;
          }
        }
        ## finishing any existing payload
        if ( in.chunk ) res[[cur.field]] <- end.chunk(cur.field,payload)
        in.chunk <- TRUE
        cur.field <- new.field
        payload <- new.contents
        ##note<< The "value" section of a .Rd file is implicitly a describe
        ## block and \code{##}\code{value}\code{<<} acts accordingly. Therefore
        ## it automatically enables the describe block itemization (##<< after
        ## list entries).
        if ( "value" == new.field ){
          first.describe <- TRUE;
        }
      }
    } else if ( in.chunk && grepl(cont.re,line,perl=TRUE) ){
      ## append this line to current chunk
      if ( !grepl(prefix,line,perl=TRUE) ){
        ##describe<< Any lines with "\code{### }" at the left hand
        ## margin within the included chunks are handled separately,
        ## so if they appear in the documentation they will appear
        ## before the \code{##}\emph{xxx}\code{<}\code{<} chunks.
### This one should not appear.
        stripped <- gsub(cont.re,"",line,perl=TRUE)
        if ( nzchar(payload) ){
          payload <- paste(payload,stripped,sep="\n")
        } else {
          payload <- stripped
        }
      }
    } else if ( grepl(arg.pat,line,perl=TRUE) ){
      not.describe <- (0==in.describe && !first.describe)
      if ( in.chunk && not.describe){
        res[[cur.field]] <- end.chunk(cur.field,payload)
      }
      comment <- gsub(arg.pat,"\\3",line,perl=TRUE);
      arg <- gsub(arg.pat,"\\\\item\\{\\1\\}",line,perl=TRUE)
      in.chunk <- TRUE
      if ( not.describe ){
        ## TDH 2010-06-18 For item{}s in the documentation list names,
        ## we don't need to have a backslash before, so delete it.
        arg <- gsub("^[\\]+","",arg)
        #cur.field <- gsub("...","\\dots",arg,fixed=TRUE) ##special case for dots
        cur.field <- arg 
        payload <- comment
      } else {
        ## this is a describe block, so we need to paste with existing
        ## payload as a new \item.
        if ( first.describe ){
          ## for first item, need to add describe block starter
          if ( "value" == cur.field ){
            payload <- paste(payload,"\n",arg,"{",sep="")
          } else {
            payload <- paste(payload,"\\describe{\n",arg,"{",sep="")
          }
          first.describe <- FALSE
          in.describe <- in.describe+1
        } else {
          ## subsequent item - terminate existing and start new
          payload <- paste(payload,"}\n",arg,"{",sep="")
        }
        if ( nzchar(comment) ){
          payload <- paste(payload,comment,sep="")
        }
      }
    } else if ( in.chunk ){
      if ( 0 == in.describe && !first.describe ){
        ## reached an end to current field, but need to wait if in.describe
        res[[cur.field]] <- end.chunk(cur.field,payload)
        in.chunk <- FALSE
        cur.field <- NULL
        payload <- NULL
      }
    }
    k <- k+1
  }
  ## finishing any existing payload
  if ( 0 < in.describe ){
    if ( "value" != cur.field ){    # value is implicit describe block
      payload <- paste(payload,"}",sep="")
    }
    while ( in.describe>0 ){
      payload <- paste(payload,"}",sep="\n")
      in.describe <- in.describe-1;
    }
  }
  if ( in.chunk ) res[[cur.field]] <- end.chunk(cur.field,payload)
  res
### Named list of character strings extracted from comments. For each
### name N we will look for N\{...\} in the Rd file and replace it
### with the string in this list (implemented in modify.Rd.file).
}
