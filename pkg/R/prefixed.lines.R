#
# vim:set ff=unix expandtab ts=2 sw=2:

prefixed.lines <- function(src,...){
### The primary mechanism of inline documentation is via consecutive
### groups of lines matching the specified prefix regular expression
### "\code{^### }" (i.e. lines beginning with "\code{### }") are
### collected as follows into documentation sections:\describe{
### \item{description}{group starting at line 2 in the code}
### \item{arguments}{group following each function argument}
### \item{value}{group ending at the penultimate line of the code}}
### These may be added to by use of the \code{##<<} constructs
### described below.
  clines <- grep(prefix,src)
  if(length(clines)==0)return(list())
  bounds <- which(diff(clines)!=1)
  starts <- c(1,bounds+1)
  ends <- c(bounds,length(clines))
  ## detect body of function using paren matching
  code <- gsub("#.*","",src)
  f <- function(ch)cumsum(nchar(gsub(sprintf("[^%s]",ch),"",code)))
  parens <- f("(")-f(")")
  body.begin <- which(diff(parens)<0 & parens[-1]==0)+2
  if(length(body.begin)==0)body.begin <- 1 ## rare cases
  is.arg <- function(){
    gres <- grep("^\\s*#",src[start-1],perl=TRUE)
    0 == length(gres) && start<=body.begin
  }
  res <- list()
  for(i in seq_along(starts)){
    start <- clines[starts[i]]
    end <- clines[ends[i]]
    processed <- gsub("#.*","",gsub("[ }]","",src[(end+1):length(src)]))
    lab <- if(all(processed==""))"value"
    else if(start==2)"description"
    else if(is.arg()){
      ##twutz: strip leading white spaces and brackets and ,
      arg <- gsub("^[ \t(,]*", "", src[start - 1])
      arg <- gsub("^([^=,]*)[=,].*", "\\1", arg)
      ##twutz: remove trailing whitespaces
      arg <- gsub("^([^ \t]*)([ \t]+)$","\\1",arg)
      arg <- gsub("...", "\\dots", arg, fixed = TRUE)
      ##fixme:mm
      ## I would rather avoid wrapping the names of the function arguments
      ## in "item{$arg}" but save them under their real name $arg
      ## this would avoid having to unpack them later
      paste("item{",arg,"}",sep="")
    } else {
      next;
    }
    res[[lab]] <- decomment(src[start:end])
  }
  res
}
