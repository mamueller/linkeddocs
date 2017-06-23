
## vim:set ff=unix expandtab ts=2 sw=2:
### Method signatures can be long when transformed to character
### unfortunately too long to be accepted by cran 
### due to limitations of the  'tar ' program which is used to build the package
### This function computes a hash for the signature which is much shorter and 
### also unique (at least with with very high probality)

uniqueMethodFileNameTrunk <- function( # filennametrunk for method Rd and  example files
	obj ##<< of class MethodDefinition
){
		
    sig <- obj@defined
    genName <- obj@generic
    fixPackageFileNames(paste(genName,"-method_",digest(as.character(sig),alg='crc32'),sep=""))
}
