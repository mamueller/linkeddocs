#!/usr/bin/Rscript
require(devtools)
devtools::install('~/linkeddocs/pkg/')
devtools::install('~/R6Unit/pkg')
#devtools::install_cran('getopt')
require(getopt)
print(get_Rscript_filename())
