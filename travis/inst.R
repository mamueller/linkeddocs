#!/usr/bin/Rscript

fromCran <- c('stringr','devtools','remotes','pkgload','deSolve')
lapply(fromCran,install.packages,repos='https://cran.uni-muenster.de')
#
fromGitHub <- c('debugHelpers','R6Unit')
require(remotes)
lapply(fromGitHub,function(name){remotes::install_github(sprintf("mamueller/%s/pkg",name))})

# install the package itself from source  
#devtools::install(file.path('/home/mm/R6Unit/pkg'))
devtools::install(file.path('pkg'))
