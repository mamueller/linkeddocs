
# vim:set ff=unix expandtab ts=2 sw=2:
createObjects <- function(code){
  ### the function creates the environment object lists and expression by parsing all the code files
  ### Is is factored out to make writing tests easier
  ### since we often need the objects and the environment 
  ### they inhabit 
  e <- new.env()

  #all <- devtools::load_all()
  #e <- all[['env']]

  ## KMP 2011-03-09 fix problem with DocLink when inlinedocs ran on itself
  ## Error in assignClassDef(Class, classDef, where) :
  ##   Class "DocLink" has a locked definition in package "inlinedocs"
  ## Traced to "where" argument in setClassDef which defaults to topenv()
  ## which in turn is inlinedocs when processing inlinedocs package, hence
  ## the clash. The following works (under R 2.12.2), so that the topenv()
  ## now finds e before finding the inlinedocs environment.
  
  #old <- options(keep.source=TRUE,topLevelEnvironment=e)
  old <- options(topLevelEnvironment=e)
  on.exit(options(old))
  exprs <- parse(text=code,keep.source=TRUE)
  ## TDH 2011-04-07 set this so that no warnings about creating a fake
  ## package when we try to process S4 classes defined in code
  #e$.packageName <- "inlinedocs.processor"
  e$.packageName <- fakePackageName()
  for (i in exprs){
      eval(i, e)
  }
  objs <- sapply(ls(e),get,e,simplify=FALSE) # note that ls will not find S4 classes nor methods for generic functions
  list(objs=objs,env=e,exprs=exprs)
}
