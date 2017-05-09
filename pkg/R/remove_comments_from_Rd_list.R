#
# vim:set ff=unix expandtab ts=2 sw=2:
remove_comments_from_Rd_list<-function
	### The Rd list templates created with \code{utils::prompt()}
	### add a lot of comments hinting at information
	### the user has to fill in
  ### This function removes these comments 
  ### Another option is to avoid using \code{prompt} entirely
  ### and replace it by something that only writes what we want
	(
	  Rd_list ##<< The list like Rd object returned by e.g. \code{promp(f,filename=NA)}
	)
	{
    for (n in names(Rd_list)){
      dlines<-Rd_list[[n]]
      comments <- grep("^[%~]",dlines)
      if ( 0 < length(comments) ){
        Rd_list[[n]]<- dlines[-comments]
      }
    }
		return(Rd_list)
	}
