#
# vim:set ff=unix expandtab ts=2 sw=2:
sigsList <- function(g, where) {
    methods <- findMethods(g, where)
    value <- methods@signatures
    args <- methods@arguments
    if (length(value)) {
        length(args) <- length(value[[1]])
        value <- lapply(value, function(x) {
            names(x) <- args
            x
        })
    }
    value
}
