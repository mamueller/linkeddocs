assertCranResultOk=function(l,msg="devtools::check failed"){
  ne<-length(l$errors)
  nw<-length(l$warnings)
  nn<-length(l$notes)
  tn<-ne+nw+nn
  cond<-(ne+nw+nn)>0
  print(cond)
  if(cond){
    print(l)
  }
  self$assertEqual(ne,0)
  self$assertEqual(nw,0)
  self$assertEqual(nn,0)
}
