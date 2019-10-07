#
## vim:set ff=unix expandtab ts=2 sw=2:
require(R6Unit)
      test<-function(){
        print("############# here I am ##########")
        #stop('to fail')
      }
MMTest<-R6Class("MMTest",
	#inherit=InDirTest,
	inherit=TestCase,
  public=list(
    targetPkgName=""
    ,
    #----------------
    cp_package_files=function(targetPkgName){
      test()
    }
    ,
    test.title=function(){
      self$cp_package_files("ClassWithMethods")
      self$assertTrue(TRUE)
    }
  )
)
