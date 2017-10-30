#!/usr/bin/Rscript 
# vim:set ff=unix expandtab ts=2 sw=2:

#source("R6_prolog.R")
require(R6Unit)
require(devtools,quiet=TRUE)
#devtools::install('../..')
require(linkeddocs)
#s<-get_suitefromDiscoveredTestInstances(".","^r6unit.*.R")
source("PackageTests_3.R")
source("PrototypeTests.R")
source("ExampleExtractionTest.R")
source("ClassDocTest.R")

s<-TestSuite$new(list(
	# TestResults$new()
	# ,
	# ClassDocTest$new("test.Rd_constructor_lines_for_virtual_class")
	# ,
	# ClassDocTest$new("test.AutoConstructor_lines")
	# ,
	# ClassDocTest$new("test.ClassDocXXX")
	# ,
	# ClassDocTest$new("test.title")
	# ,
	# ClassDocTest$new("test.Rd_method_lines")
	# ,
	# ClassDocTest$new("test.superclass_lines")
	# ,
	# ClassDocTest$new("test.FindAutoConstructor")
	# ,
	# ClassDocTest$new("test.subclass_lines")
	# ,
	# ClassDocTest$new("test.write_Rd_file")
	# ,
	# PackageTests_3$new("test.ClassWithExamples")
	# ,
	# ExampleExtractionTest$new("test.example_references")
	# ,
	# ExampleExtractionTest$new("test.external_example_lines_for_setClass")
	# ,
	# ExampleExtractionTest$new("test.external_example_lines_for_function")
	# ,
	# ExampleExtractionTest$new("test.extract_function_body_with_comments")
	# ,
	# PrototypeTests$new("test.package.skeleton.dx_3")
	# ,
	# PrototypeTests$new("test.correctNameSpaceInfo")
	# ,
	# PackageTests_3$new('test.ClassWithMethodsAndExampleFiles')
	# ,
	# PackageTests_3$new('test.ClassWithMethods')
	# ,
	# PackageTests_3$new('test.AutoConstructor')
	# ,
	# PackageTests_3$new('test.PrivateAndPublic')
	# ,
	# PackageTests$new("test.NameSpaceParsing")
	# ,
	 PackageTests_3$new("test.SoilR")
	# ,
	# PackageTests_3$new("test.VirtualClass")
	# ,
	# PackageTests_3$new("test.AutoConstructor")
))
print(s$test_names())
s$parallel <- 1 
tr <- s$run()
tr$summary()

