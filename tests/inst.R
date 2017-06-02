#!/usr/bin/Rscript
if (!is.element('devtools',installed.packages())){
	install.packages('devtools',repos='https://cran.uni-muenster.de')
}
require(devtools)
devtools::install('~/linkeddocs/pkg/')
devtools::install('~/R6Unit/pkg')
require(getopt)
print(get_Rscript_filename())
