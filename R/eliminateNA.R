`eliminateNA` <-
function (dat) 
{
    n <- dim(dat)[1]
    c <- dim(dat)[2]
    tmp <- matrix(NA, ncol = c, nrow = n)
    for (i in 1:c) {
        tmp[, i] <- as.numeric(dat[, i])
    }
    compl <- sign(apply(tmp, 1, sum))
    compl <- (1:n) * compl
    incompl <- (1:n) * (is.na(compl) == TRUE)
    incompl <- incompl[incompl > 0]
    compl <- compl[is.na(compl) == FALSE]
    return(list(complete = dat[compl, ], incomplete = dat[incompl, 
        ]))
}
