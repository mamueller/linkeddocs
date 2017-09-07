
methodDefs <- function(parsed){
  parsed[sapply(parsed,isSetMethodExpression)]
}
