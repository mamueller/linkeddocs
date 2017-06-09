#
# vim:set ff=unix expandtab ts=2 sw=2:
#################################################################
writeGenericFunctionRdFiles <- function(path,docs,nsi){
  gens<-nsi[["gens_defined_by_package"]]
  funNamesToDocument  <-names(gens) 
  #funNamesToDocument  <-names(nsi[["documentableMeths"]]) 
  # note that ls will not find S4 methods for generic functions 
  # these are treated elsewhere
  list0 <- fixPackageFileNames(funNamesToDocument)
  names(list0) <- funNamesToDocument
  sapply(
    funNamesToDocument, 
    function(item) {
      docs_i<-docs[[item]]
      if (!is.null(docs_i)){
        fn <- file.path(path, paste(list0[[item]],".Rd",sep=""))
        #fff<-getGeneric(item,where=e)
        fff<-gens[[item]]
        fdo=genericFunctionDocObject(name=item,l=docs_i,functionObject=fff)
        write_Rd_file(fdo,fn)
      }else{
        # there are two possibilities 
    
        warning(sprintf("## mm ## No documentation found for item:%s.",item))
      }
    }
  )
  
  
}
