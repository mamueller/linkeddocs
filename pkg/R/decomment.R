decomment <- function
### Remove comment prefix and join lines of code to form a
### documentation string.
(comments
### Character vector of prefixed comment lines.
 ){
  gsub(prefix,"",comments)
### String without prefixes or newlines.
}
