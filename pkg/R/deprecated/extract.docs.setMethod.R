#
# vim:set ff=unix expandtab ts=2 sw=2:
extract.docs.setMethod<- function # S4 mehtod inline documentation
### Using the same conventions as for functions, definitions of S4 methods
### in the form \code{setMethod(\dots)} are also located and
### scanned for inline comments.

(doc.link,
### DocLink object as created by \code{extract.file.parse}.
 env,
 ### environment to find method source
inlinedocs.exampleDir,
### A string pointing to the location where inlinedocs should search for external examples
inlinedocs.exampleTrunk
### A regular expression used to identify the files containing external examples in the example directory
 ){
  funcSource=getMethodSrc(doc.link,env)
  method.name=getMethodName(doc.link,env)
  docs=list()
  docs<- combine(docs,prefixed.lines(funcSource))
  docs <- combine(docs,extract.xxx.chunks(funcSource,method.name))
  docs <- combine(docs,title.from.firstline(funcSource,method.name))
  docs <- combine(docs,mm.examples.from.testfile(method.name,inlinedocs.exampleDir,inlinedocs.exampleTrunk))
  docs
}
