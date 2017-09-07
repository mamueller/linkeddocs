
### Names of Parser Functions that operate on the desc arg.
descfile.names <- c("author.from.description","edit.package.file")

### Names of Parser Functions that do NOT use the desc arg.
non.descfile.names <-
  names(default.parsers)[!names(default.parsers)%in%descfile.names]

### Parsers that operate only on R code, independently of the
### description file.
nondesc.parsers <- default.parsers[non.descfile.names]

