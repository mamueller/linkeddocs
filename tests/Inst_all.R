#!/usr/bin/Rscript
if (!is.element('devtools',installed.packages())){
	install.packages('devtools',repos='https://cran.uni-muenster.de')
}
require(devtools)
#devtools::install('~/debugHelpers/pkg')
devtools::install('~/R6Unit/pkg',build=FALSE)
#devtools::install('~/linkeddocs/pkg/')
require(getopt)
print(get_Rscript_filename())
