#
# vim:set ff=unix expandtab ts=2 sw=2:
setClass("DocLink", # Link documentation among related functions
### The \code{.DocLink} class provides the basis for hooking together
### documentation of related classes/functions/objects. The aim is that
### documentation sections missing from the child are inherited from
### the parent class.
         representation(name="character", ##<< name of object
                        created="character", ##<< how created
                        parent="character", ##<< parent class or NA
                        code="character", ##<< actual source lines
                        description="character") ##<< preceding description block
         )
#-------------------------------------------------------------------------
setMethod(
  f="write_Rd_file",
  signature=signature(obj="DocLink",fn="character"),
  def=function(
      obj,
      fn
    ){
    # this method does nothing and serves as a placeholder
    # it has to be implemented in the decendents
  }
)
