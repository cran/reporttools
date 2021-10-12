addLineBreak <- function(tab, length, col){

  out.row <- ceiling(nchar(tab[, col]) / length)
  out.row <- sum(out.row) + sum(out.row == 0) + nrow(tab)
  res.mat <- matrix(NA, ncol = ncol(tab), nrow = out.row)
  iter <- 1
  iters <- c()
  
  for (i in 1:nrow(tab)){
    a <- tab[i, col]
    b <- floor(nchar(a) / length)
    c <- matrix(NA, nrow = b + 1, ncol = 1)
    for (j in 1:b){c[j, ] <- substr(a, (j - 1) * length + 1, j * length)}
    if (b * length + 1 < nchar(a)){c[j + 1, ] <- substr(a, b * length + 1, nchar(a))}
  
    ind1 <- (1:ncol(tab))[1:ncol(tab) != col]
    res.mat[iter, ind1] <- tab[i, ind1]
  
    ind2 <- iter + (0:(nrow(c) - 1))
    res.mat[ind2, col] <- c
  
    iter <- iter + b + 1
    iters[i] <- iter - 1
  }

  res.mat <- res.mat[apply(is.na(res.mat), 1, sum) < ncol(tab), ]
  res <- list(res.mat, iters)
  return(res)  

}
