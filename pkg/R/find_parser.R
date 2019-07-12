

parser_setRefClass <- function(call, env, block) {
  name <- as.character(call$Class)
  value <- methods::getClass(name, where = env)

  object(value)
}

parser_setGeneric <- function(call, env, block) {
  name <- as.character(call$name)
  value <- methods::getGeneric(name, where = env)

  object(value)
}

parser_setMethod <- function(call, env, block) {
  name <- as.character(call$f)
  value <- methods::getMethod(name, eval(call$signature), where = env)
  value@.Data <- extract_method_fun(value@.Data)

  object(value)
}

parser_setReplaceMethod <- function(call, env, block) {
  name <- paste0(as.character(call$f), "<-")
  value <- methods::getMethod(name, eval(call[[3]]), where = env)
  value@.Data <- extract_method_fun(value@.Data)

  object(value)
}

`parser_::` <- function(call, env, block) {
  pkg <- as.character(call[[2]])
  fun <- as.character(call[[3]])

  object(list(pkg = pkg, fun = fun), alias = fun, type = "import")
}

parser_setMethodS3 <- function(call, env, block) {
  # R.methodsS3::setMethodS3(name, class, ...)
  method <- as.character(call[[2]])
  class <- as.character(call[[3]])
  name <- paste(method, class, sep=".")
  value <- standardise_obj(name, get(name, env), env, block)
  object(value, name)
}

parser_setConstructorS3 <- function(call, env, block) {
  # R.oo::setConstructorS3(name, ...)
  name <- as.character(call[[2]])
  value <- standardise_obj(name, get(name, env), env, block)
  object(value, name)
}
parser_setClass <- function(call, env, block) {
  name <- as.character(call$Class)
  value <- methods::getClass(name, where = env)

  object(value)
}

parser_setClassUnion <- function(call, env, block) {
  name <- as.character(call$name)
  value <- methods::getClass(name, where = env)

  object(value)
}
parser_assignment <- function(call, env, block) {
  name <- as.character(call[[2]])

  # If it's a compound assignment like x[[2]] <- ignore it
  if (length(name) > 1)  return()

  # If it doesn't exist (any more), don't document it.
  if (!exists(name, env)) return()

  value <- get(name, env)
  value <- standardise_obj(name, value, env, block)

  object(value, name)
}
find_parser <- function(name) {
  if (name %in% c("=", "<-", "<<-")) name <- "assignment"

  parser_name <- paste0("parser_", name)
  if (!exists(parser_name)) return(NULL)

  get(parser_name, mode = "function")
}
