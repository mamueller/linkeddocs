require(methods)
autoMarkus<-setClass('Markus',slots=c(t='numeric'))
setGeneric('Markus',def=function(a,b){standardGeneric('Markus')},valueClass='Markus')
setMethod('Markus',signature=c('numeric','missing'),definition=function(a){new("Markus",t=a)})
setMethod('Markus',signature=c('character','numeric'),definition=function(a,b){new("Markus",t=b)})
#Markus<- as(Markus,"classGeneratorFunction")
#Markus@className <- 'Markus'
#Markus@package<- 'SoilR'              

print(Markus(2))
print(Markus("vaafaf",3))

print(class(Markus))
slotNames(Markus)
Markus@valueClass

