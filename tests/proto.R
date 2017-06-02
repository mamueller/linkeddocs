pkgDir='IoTestResults_tmp/PrototypeTests.test.correctNameSpaceInfo/pkg/'
devtools::install(pkgDir,keep_source = TRUE)
library('ClassWithMethods')
md=findMethods('[',asNamespace('ClassWithMethods'))
mds=md[[1]]
srcRef=utils::getSrcref(mds)
codeText=as.character(srcRef)
expr=parse(text=codeText)
print(expr)
e=new.env()
eval(expr,envir = e)
ls(e)