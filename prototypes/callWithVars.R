callWithVars <- function(workerFunc,varNames,...){
	a <- 1
	b <- 2
	c <- 3
	e <- environment()
	values <- c(as.list(e)[varNames],list(...))
	funcCall <- as.call(append(list(workerFunc),values))
	res <- eval(funcCall)
	res
}
# a function that uses only a
w1<- function(a){print(a)}
print(callWithVars(w1,c('a')))

# a function that uses a and c
w2<- function(a,c){print(a+c)}
print(callWithVars(w2,c('a','c')))

# a function that uses a,c and additional normal arguments (NOT provided by
# callWithVars but)
myVal <- 'normal param'
w3<- function(a,c,val){print(a+c);print(val)}
res <- callWithVars(w3,c('a','c'),myVal)

