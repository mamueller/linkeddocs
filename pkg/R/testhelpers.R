#
# vim:set ff=unix expandtab ts=2 sw=2:
require(stringr)
#####################################################################################################
pp=function(# print out an 
### This function is used to print out a value of a variable together with its name and is helpful for debugging
string,env=parent.frame()){
  cat("\n########################################################################\n")
  callingFun <-as.list(sys.call(-1))[[1]]
  callerName<-toString(callingFun)
  print(paste("pp in",callerName,string,":="))
  print(get(string,env))
}
#####################################################################################################
pe=function(string,env=parent.frame()){
#pe=function(string,env=parent.env(environment())){
### This function is used to print out a value of an quoted expression together with it and is helpful for debugging
  cat("\n########################################################################\n")
  callingFun <-as.list(sys.call(-1))[[1]]
  callerName<-toString(callingFun)
  print(paste("pe in",callerName,string,": =",toString(eval(string,env))))
}
#####################################################################################################
trimmedNonEmptyLines=function(s){
### a helper function to make text comparison of actual and expacted output less painfull by removing whitespace
  t=str_trim(unlist(str_split(s,"\n")))
  ttr=t[nchar(t)>0]
  return(ttr)
}
#####################################################################################################
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
#####################################################################################################
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
