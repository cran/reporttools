displayCI <- function(ci, digit = 2, unit = ""){
    ci <- format(round(ci, digit), nsmall = digit)
    d <- 1
    if (is.matrix(ci) == TRUE){d <- nrow(ci)} else {ci <- matrix(ci, ncol = 2)}
    ci <- sub(' ', '', ci)    # eliminate spaces
    disp.ci <- rep(NA, d)
    for (i in 1:d){disp.ci[i] <- paste("[", ci[i, 1], unit, ", ", ci[i, 2], unit, "]", sep = "")}
    return(disp.ci)
}
