
## vim:set ff=unix expandtab ts=2 sw=2:
require(R6Unit,quiet=TRUE)
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
require(stringr)
pp=function(# print out an 
### This function is used to print out a value of a variable together with its name and is helpful for debugging
string,env=parent.frame()){
  cat("\n########################################################################\n")
  callingFun <-as.list(sys.call(-1))[[1]]
  callerName<-toString(callingFun)
  print(paste("pp in",callerName,string,":="))
  print(get(string,env))
}
#------------------------------------------------------------------------------
pe=function(string,env=parent.frame()){
#pe=function(string,env=parent.env(environment())){
### This function is used to print out a value of an quoted expression together with it and is helpful for debugging
  cat("\n########################################################################\n")
  callingFun <-as.list(sys.call(-1))[[1]]
  callerName<-toString(callingFun)
  res <- toString(eval(string,env))
  out <- sprintf('pe in %s:\nExpression\t: %s\nResult\t\t: %s \n',callerName,as.expression(string),res)
  cat(out)
  cat("\n########################################################################\n")
}
#------------------------------------------------------------------------------
trimmedNonEmptyLines=function(s){
### a helper function to make text comparison of actual and expacted output less painfull by removing whitespace
  t=str_trim(unlist(str_split(s,"\n")))
  ttr=t[nchar(t)>0]
  return(ttr)
}
#------------------------------------------------------------------------------
CompareTrimmedNonEmptyLines=function
### helper function needed for comparison of doc files
(s1,s2){
  t1=trimmedNonEmptyLines(s1)
  t2=trimmedNonEmptyLines(s2)
  l1=length(t1)
  l2=length(t2)
  if (l1!=l2){
    print(paste("The number of lines differs l1=",l1,"  l2=",l2 ))
    return(FALSE)
  }
  for(i in (1:l1)){
    ti1=t1[[i]]
    ti2=t2[[i]]
    if (ti1!=ti2) {
      print("\n")
      print(ti1)
      print(ti2)
      print(paste("line",i,"does not match"))
      return(FALSE)
    }
  }
  return(TRUE)
}
#------------------------------------------------------------------------------
  .lineSplitter=function(line,sep,pos){if (nchar(line)>pos){line=sub(sep,paste(sep,"\n",sep=""),line)};line}
  .textSplitter=function(utxt,sep,pos){
    Lines=unlist(strsplit(utxt,"\n"))
    utxt=paste0(unlist(lapply(Lines,.lineSplitter,sep,pos)),collapse="\n")
    utxt
  }
  .widthCutter=function(utxt,pos){
    newtxt=.textSplitter(utxt,",",pos)
    newtxt=.textSplitter(newtxt,"\\(",pos)
    newtxt=.textSplitter(newtxt,"\\)",pos)
    if (newtxt==utxt){
      return(utxt)
    }else{
      return(.widthCutter(newtxt,pos))
    }
  }
#------------------------------------------------------------------------------
tryTwice <- function(qe){
  print('first Evaluation')
  res <- tryCatch(
  	eval(qe),
	error=function(e){e}
  )
  pp('res')
  if(inherits(res,'simpleError')){
  	print('second Evaluation')
  	res <- eval(qe)
  }
  return(res)
}
