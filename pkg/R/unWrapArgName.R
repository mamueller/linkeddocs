# fixme mm:
# This function unwraps the name of a function argument 
# "item{arg}" into "arg"
# "item{arg} is the common form in Rd file 
# and unfortunately also in inlinedocs documentation list for a function
# I would rather avoid it until the Rd file is finally written.
# I had to create it to avoid duplication since it is needed
# in more than one place
unWrapArgName=function(wrapped_arg_name){ sub("[}]","",sub("item[{]","",wrapped_arg_name)) }

