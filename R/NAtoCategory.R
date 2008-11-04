`NAtoCategory` <-
function (fact, label = "missing") 
{
    res <- fact
    if (sum(is.na(fact)) > 0) {
        leve <- levels(fact)
        leve <- leve[leve != ""]
        res <- as.numeric(fact)
        res[is.na(res) == TRUE] <- max(res, na.rm = TRUE) + 1
        res <- factor(res, levels = names(table(res)), labels = c(leve, 
            label))
    }
    return(res)
}
