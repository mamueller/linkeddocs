#!/usr/bin/Rscript 
# vim:set ff=unix expandtab ts=2 sw=2:

require(R6Unit)
require("pkgload")
pkgload::load_all('../pkg',export_all=FALSE)
#pkgload::load_all('~/debugHelpers/pkg',export_all=FALSE)
#pkgload::load_all('~/R6Unit/pkg',export_all=FALSE)
#source("ClassDocScriptTest.R")
source("PkgScriptTests.R")

s<-TestSuite$new(list(
	# TestResults$new()
	# ,
	# ClassDocTest$new("test.Rd_constructor_lines_for_virtual_class")
	# ,
	# ClassDocTest$new("test.AutoConstructor_lines")
	# ,
	#ClassDocScriptTest$new("test.title")
	# ,
	#ClassDocTest$new("test.title")
  #,
  PkgScriptTests$new("test.abbriviatedSignature")
	#ClassDocTest$new("test.classGeneratorFunction")
	#ClassDocScriptTest$new("test.classGeneratorFunction")
	#MMTest$new("test.title")
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
	# PackageTests_3$new("test.MethodSrcRef")
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
	# PackageTests_3$new("test.SoilR")
	# ,
	# PackageTests_3$new("test.VirtualClass")
	# ,
	# PackageTests_3$new("test.AutoConstructor")
	# S4MethodDocTest$new("test.SetMethod_lines")
))
print(s$test_names())
#s$parallel <- 1 
tr <- s$run()
tr$print_summary()

