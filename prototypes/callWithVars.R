callWithVars <- function(workerFunc,varNames){
	a <- 1
	b <- 2
	c <- 3
	e <- environment()
	cl <- call('workerfunc')
	values <- as.list(e)[varNames]
	funcCall <- as.call(append(list(workerFunc),values))
	res <- eval(funcCall)
	res
}
w1<- function(a){print(a)}
print(callWithVars(w1,c('a')))

w2<- function(a,c){print(a+c)}
print(callWithVars(w2,c('a','c')))
