#!/usr/bin/Rscript
requireNamespace('linkeddocs')
pkgload::load_all('pkg')
linkeddocs::package.skeleton.dx_3(file.path('examplePackages','PackageTests_3.test.ClassWithMethods','pkg'))
