`display2by2` <-
function (v1, v2, names = c("v1", "v2"), cap = "", lab = "", 
    row.nam = NA, col.nam = NA) 
{
    mat <- table(v1, v2)
    if (is.na(row.nam[1]) == TRUE) {
        row.nam <- dimnames(mat)[[1]]
    }
    if (is.na(col.nam[1]) == TRUE) {
        col.nam <- dimnames(mat)[[2]]
    }
    s1 <- as.vector(apply(mat, 2, sum))
    s2 <- as.vector(apply(mat, 1, sum))
    s3 <- sum(mat)
    mat <- rbind(col.nam, mat, s1)
    mat <- cbind(c(NA, row.nam, "Total"), mat)
    mat <- cbind(mat, c("Total", s2, s3))
    mat <- rbind(c(NA, names[2], NA, NA), mat)
    mat <- cbind(c(NA, NA, names[1], NA, NA), mat)
    mat <- data.frame(mat)
    xtab2 <- xtable(mat, align = "lll|cc|c", caption = cap, label = lab)
    print(xtab2, include.rownames = FALSE, include.colnames = FALSE, 
        floating = FALSE, hline.after = c(0, 2, 4), type = "latex", 
        size = "footnotesize", sanitize.text.function = function(x) {
            x
        }, table.placement = "h!", tabular.environment = "longtable")
}
