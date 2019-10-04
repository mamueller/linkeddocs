#!/usr/bin/Rscript
if (!is.element('devtools',installed.packages())){
	install.packages('devtools',repos='https://cran.uni-muenster.de')
}
require(devtools)
#devtools::install_github("tidyverse/dplyr")
#devtools::install_github("mamueller/R6Unit",subdir='pkg')
#devtools::install_github("mamueller/debugHelpers",subdir='pkg')
devtools::install("~/debugHelpers/pkg",quick=TRUE)
#devtools::install('~/linkeddocs/pkg/',quick=TRUE)
require(getopt)

