
## vim:set ff=unix expandtab ts=2 sw=2:
library(R6Unit,quiet=TRUE)
require(devtools,quiet=TRUE)
require(getopt,quiet=TRUE)
mmsg=function(){"################## mm ####################"}
#------------------------------------------------------------------------------
writeDescriptionFile<-function(Depends=NULL,pkgName="ExamplePackage",pkgDir="."){
	desc <-paste("Package:",pkgName," 
Title: EXAMPLES to TEST the POSSIBILITIES of NAMESPACES  
Version:0.1
Date: ",Sys.Date(),"
Author:  Markus Mueller <mamueller@bgc-jena.mpg.de>
Maintainer: Markus Mueller <mamueller@bgc-jena.mpg.de>
Description: This package contains functions whose automatic documentation is tested.
License: GPL-3
",
ifelse(is.null(Depends),"",paste("Depends:",toString(Depends)))
	,sep="")
	descFilePath=file.path(pkgDir,"DESCRIPTION")
	cat(desc,file=descFilePath)
}
#------------------------------------------------------------------------------
cpDir<-function(sourceDirPath,targetDirPath){
  # copy a directory tree recursively  
	all_entries<-list.files(sourceDirPath)
	all_dirs<-all_entries[
    as.logical(lapply(all_entries,function(entry){dir.exists(file.path(sourceDirPath,entry))}))
    ]
	all_files<-setdiff(all_entries,all_dirs)
	
  if (!dir.exists(targetDirPath)){
    dir.create(targetDirPath)
  }
  # first we copy files (this would however not include the empty dirs))
	lapply(
		all_files,
		function(fp){
			file.copy(file.path(sourceDirPath,fp),file.path(targetDirPath,fp))
		}
	)
  
  lapply(
		all_dirs,
		function(subDirName){
			cpDir(file.path(sourceDirPath,subDirName),file.path(targetDirPath,subDirName))
		}
  )
}
#------------------------------------------------------------------------------
