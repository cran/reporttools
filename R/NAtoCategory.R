`NAtoCategory` <-
function (fact, label = "missing") 
{
    res <- fact
    if (sum(is.na(fact)) > 0) {
        leve <- levels(fact)
        leve <- leve[leve != ""]
        res <- as.numeric(fact)
        res[is.na(res) == TRUE] <- length(leve) + 1
        res <- factor(res, levels = 1:(length(leve) + 1), labels = c(leve, 
            label))
    }
    return(res)
}
