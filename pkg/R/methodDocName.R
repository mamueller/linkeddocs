
# vim:set ff=unix expandtab ts=2 sw=2:
methodDocName=function
### creates the actual *.Rd filename for a method from its signature and the generic it implements
(genName,sig){
  #N=paste(genName,"_method__",sigString(sig),sep="")
  sigString<-paste0(sig[names(sig)],collapse=",")
  N=sprintf("%s,%s-method",genName,sigString)
  N
}
