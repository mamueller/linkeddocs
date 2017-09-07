
# we now want to find all Generics that have at least one Method where we can get at the source
############################################################
methSrc=function
### get at the src of a method given as  an MethodDefinition object
(MethodDefinition){getSrcref(unRematchDefinition(MethodDefinition))}
