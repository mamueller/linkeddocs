
### Since this class is exported in the Namespace file you can inherit from it
setClass(# an Exposed  class
   Class="ExposedVirtualClass",
   contains='VIRTUAL',
   slots=c(times="numeric")
)

### This is a virtual class and it will be detected automatically
setClass(# an Exposed  class
   Class="ExposedClass",
   contains='ExposedVirtualClass',
   slots=c(times="numeric")
)

