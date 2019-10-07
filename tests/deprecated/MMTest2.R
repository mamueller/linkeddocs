#
## vim:set ff=unix expandtab ts=2 sw=2:
test<-function(){
  print("############# here I am ##########")
  #stop('to fail')
}
require(R6)
MMTest<-R6Class("MMTest",
	#inherit=InDirTest,
	#inherit=TestCase,
  public=list(
    targetPkgName=""
    ,
    #----------------
    help=function(){
      test()
    }
    ,
    run=function(name){
    funtoTest=as.list(self)[[name]]
    require(parallel)
    n<-min(detectCores(),2)
    cl<-makePSOCKcluster(n)
    funcs=c(funtoTest)
    resultList <- clusterApply(cl,funcs,function(f){f()})
    #funtoTest()
    }
  )
)
MMTestChild<-R6Class("MMTestChild",
	inherit=MMTest,
  public=list(
    targetPkgName=""
    ,
    #----------------
    test.title=function(){
      self$help()
    }
  )
)
mm=MMTestChild$new()
mm$run('test.title')
