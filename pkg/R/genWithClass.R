#
# vim:set ff=unix expandtab ts=2 sw=2:
genWithClass <- function(cl,where) {
    allgen <- getGenerics(where = where)
    ok <- as.logical(unlist(lapply(allgen, classInSig, cl = cl )))
    return(allgen[ok])
}
