#
# vim:set ff=unix expandtab ts=2 sw=2:
checkWarnArgs<-function(
 name,
 function_args,
 documented_args
){
    undocumented_args<-setdiff(function_args,documented_args)
    if(length(undocumented_args)!=0){
      warning(
      	sprintf("Undocumented arguments found in %s.\n %s\n %s\n %s",
          name,
		      paste("function_args=: ",toString(function_args)),
		      paste("documented_args=: ",toString(documented_args)),
		      paste("undocumented_args=: ",toString(undocumented_args))
	      )
	    )
    }
}
