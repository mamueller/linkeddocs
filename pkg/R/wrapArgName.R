
# vim:set ff=unix expandtab ts=2 sw=2:
# fixme mm:
# This function wraps the name of a function argument 
# "arg"  into "item{arg}" which is the common form in Rd file
# and also in inlinedocs documentation list for a function
# I would rather avoid it until the rd file is finally written.
# I had to create it to avoid duplication since it is needed
# in more than one place
wrapArgName=function(arg_name){sprintf("item{%s}",arg_name)}


