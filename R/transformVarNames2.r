transformVarNames2 <- function(nams){
  out <- paste(nams, " = ", nams, sep = "")
  return(cat(matrix(out, ncol = 1), sep = ",\n"))
}
