
definition.from.source=function(doc,src,...)
### small helper to extract the definition of a doc entry from a bit of src code
{
  def <- doc$definition
  is.empty <- function(x)is.null(x)||x==""
  if(is.empty(def) && !is.empty(src))
    list(definition=src)
  else list()
}
