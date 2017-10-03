fn='minitest.R'
lines <- readLines(fn)
sf <- srcfile(fn)
exprs <- parse(text=lines,srcfile=sf,keep.source=TRUE)
pe(quote(attributes(exprs)))
srcreflist <- attr(exprs,'srcref')
