#
# vim:set ff=unix expandtab ts=2 sw=2:
mask_special_Rd_characters<-function
	### some characters  cause trouble if the occour in the text
	### % is a comment in an *.Rd file and will break { ... } pairs if
	### it occuours in the middle
	(
	  Rd_lines ##<< the text to be written in the rdfile
	)
	{
		return(gsub("%","\\\\%",Rd_lines))
	}

