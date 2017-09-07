
### combine NULL objects.
combine.NULL<-function(x,y){
    if ((class(x) == "NULL")& (class(y) == "NULL")){
        # print(paste("mm x=",x))
        # print(paste("mm class(x)=",class(x)))
	return(NULL)
    }
    if (class(x) == "NULL"){
        # print(paste("mm x=",x))
        # print(paste("mm class(x)=",class(x)))
        x=list("")
    }
    if (class(y) == "NULL"){
        # print(paste("mm y=",y))
        # print(paste("mm class(y)=",class(y)))
        y=list("")
    }
    return(combine(x,y))
}
