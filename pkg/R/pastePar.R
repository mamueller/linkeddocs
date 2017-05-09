#
# vim:set ff=unix expandtab ts=2 sw=2:
pastePar <- function(x) {
    xn <- names(x)
    x <- as.character(x)
    xn <- if (length(xn) == length(x)) 
        paste(xn, "= ")
    else ""
    paste0("(", paste0(xn, "\"", x, "\"", collapse = ", "), 
        ")")
}
