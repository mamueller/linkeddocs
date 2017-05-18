#-------------------------------------------------------------------------
setMethod(
  f="write_Rd_file",
  signature=signature(obj="MethodDefinition"),
  def=function(obj){
     print(findText(obj))
  }
)
