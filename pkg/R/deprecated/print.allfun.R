### Print method for functions constructed using forall.
print.allfun <- function(x,...){
  e <- environment(x)
  cat("Function to apply to every element.\nselector:")
  print(e$subfun)
  cat("processor:")
  print(e$FUN)
}
