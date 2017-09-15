
## vim:set ff=unix expandtab ts=2 sw=2:
xxx.chunkNames<- function(){
    specialChunkNames <-  "exampleFunctionsFromFiles"
    # We accept some special comment sections of the form
    # ##name<<
    # ##
    # where "name" will not be an Rd section but the content 
    # of the comments will be extracted nethertheless but 
    # used to create the section more indirectly
   
    union(specialChunkNames ,RdTargetSections())
}
