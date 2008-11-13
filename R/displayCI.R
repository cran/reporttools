`displayCI` <-
function (ci, digit = 2) 
{
    d <- 1
    if (is.matrix(ci) == TRUE) {
        d <- nrow(ci)
    }
    else {
        ci <- matrix(ci, ncol = 2)
    }
    disp.ci <- rep(NA, d)
    for (i in 1:d) {
        disp.ci[i] <- paste("[", disp(ci[i, 1], digit), ", ", 
            disp(ci[i, 2], digit), "]", sep = "")
    }
    return(disp.ci)
}
