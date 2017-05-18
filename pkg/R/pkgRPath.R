
# vim:set ff=unix expandtab ts=2 sw=2:
pkgRPath <- function(pkgDir){
	normalizePath(file.path(pkgDir,'R'))
}
