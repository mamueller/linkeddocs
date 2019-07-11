standardise_call <- function(call, env = parent.frame()) {
  stopifnot(is.call(call))
  #mm call[[1]] is just the function name
  f <- eval(call[[1]], env)
  if (is.primitive(f)) return(call)

  match.call(f, call)
}
