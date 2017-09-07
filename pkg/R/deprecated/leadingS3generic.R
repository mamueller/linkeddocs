
leadingS3generic <- function # check whether function name is an S3 generic
### Determines whether a function name looks like an S3 generic function
(name,                     ##<< name of function
 env,                      ##<< environment to search for additional generics
 ...)                      ##<< ignored here
{
  ##details<< This function is one of the default parsers, but exposed as
  ## possibly of more general interest. Given a function name of the form
  ## x.y.z it looks for the generic function x applying to objects of class
  ## y.z and also for generic function x.y applying to objects of class z.
  ##
  parts <- strsplit(name, ".", fixed = TRUE)[[1]]
  l <- length(parts)
  if (l > 1) {
    for (i in 1:(l - 1)) {
      ## Look for a generic function (known by the system or defined
      ## in the package) that matches that part of the function name
      generic <- paste(parts[1:i], collapse = ".")
      if (any(generic %in% getKnownS3generics()) ||
          findGeneric(generic, env) != "") {
        object <- paste(parts[(i + 1):l], collapse = ".")
        ##details<< Assumes that the first name which matches any known
        ## generics is the target generic function, so if both x and x.y
        ## are generic functions, will assume generic x applying to objects
        ## of class y.z
        ##value<< If a matching generic found returns a list with a single component:
        return(list(.s3method=c(generic, object))) ##<< a character vector containing generic name and object name.
      }
    }
  }
  ##value<< If no matching generic functions are found, returns an empty list.
  list()
}
