func1<- function(){
        eci <- new(Class="ExposedClass",1:4)
        exposedGeneric(eci,1)
}
func2<- function(){
        eci <- new(Class="ExposedClass",1:4)
        exposedGeneric(eci,2)
}
