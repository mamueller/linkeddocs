
isSetMethodExpression <- function(lang){
    chars <- as.character(lang)
    expr.type <- chars[1]
    return(expr.type == "setMethod" )
}
