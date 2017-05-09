#
# vim:set ff=unix expandtab ts=2 sw=2:
classInSig <- function(g, where, cl) {
        cl %in% unique(unlist(findMethods(g, where)@signatures))
    }

