#!/usr/bin/Rscript
if (!is.element('devtools',installed.packages())){
	install.packages('devtools',repos='https://cran.uni-muenster.de')
}
require(devtools)
#devtools::install_github("tidyverse/dplyr")
succ <- devtools::install('~/linkeddocs/pkg/')
require(getopt)

