`tableDate` <-
function (vars, nams, group = NA, subset = NA, disp.cols = c("n", 
    "min", "q1", "median", "mean", "q3", "max", "na"), col.tit = NA, 
    print.pval = TRUE, cap = "", lab = "") 
{
    dc <- c("n", "min", "q1", "median", "mean", "q3", "max", 
        "na")
    disp.cols.num <- pmatch(disp.cols, dc)
    for (i in 1:length(nams)) {
        nams[i] <- gsub("_", "\\\\_", as.character(nams[i]))
    }
    if (max(is.na(subset) == FALSE) == 1) {
        group <- group[subset]
        for (v in 1:length(vars)) {
            vars[[v]] <- vars[[v]][subset]
        }
    }
    if (max(is.na(group) == TRUE) == 1) {
        group <- rep(1, length(vars[[1]]))
    }
    if (max(is.na(col.tit) == TRUE) == 1) {
        col.tit <- c("{\\bf Variable}", "{\\bf Levels}", "n", 
            "Min", "$q_1$", "$\\widetilde{x}$", "$\\bar{x}$", 
            "$q_3$", "Max", "\\#NA")
    }
    group <- as.factor(group)
    n.levels <- length(levels(group))
    n.var <- length(nams)
    out <- matrix(NA, ncol = 10, nrow = (n.levels + 1) * n.var)
    out <- data.frame(out)
    if (n.levels == 1) {
        ali <- "ll"
    }
    if (n.levels > 1) {
        ali <- "lll"
    }
    for (i in 1:n.var) {
        ind <- (i - 1) * (n.levels + 1) + 1:(n.levels + 1)
        splits <- split(vars[[i]], group)
        for (j in 1:n.levels) {
            tmp <- splits[[j]]
            out[ind[j], 3] <- sum(is.na(tmp) == FALSE)
            out[ind[j], 4:9] <- format(summary(tmp)[1:6])
            out[ind[j], 10] <- sum(is.na(tmp) == TRUE)
        }
        out[max(ind), 3] <- sum(is.na(vars[[i]]) == FALSE)
        out[max(ind), 4:9] <- format(summary(vars[[i]])[1:6])
        out[max(ind), 10] <- sum(is.na(vars[[i]]) == TRUE)
        if ((n.levels > 1) && (print.pval == TRUE)) {
            out[(i - 1) * (n.levels + 1) + n.levels + 1, 1] <- paste("p = ", 
                format.pval(kruskal.test(splits)$p.value), sep = "")
        }
    }
    align.disp.cols <- ""
    disp.cols2 <- c(1:2, 2 + disp.cols.num)
    for (i in 1:length(disp.cols.num)) {
        align.disp.cols <- paste(align.disp.cols, "r", sep = "")
    }
    out2 <- out[, disp.cols2]
    out2[((1:n.var) - 1) * (n.levels + 1) + 1, 1] <- nams
    dimnames(out2)[[2]] <- c("{\\bf Variable}", "{\\bf Levels}", 
        "n", "Min", "$q_1$", "$\\widetilde{x}$", "$\\bar{x}$", 
        "$q_3$", "Max", "\\#NA")[disp.cols2]
    tmp <- (n.levels + 1) * 1:n.var
    hlines <- sort(c(0, tmp - 1, rep(tmp, times = 2)))
    hlines <- hlines[1:(length(hlines) - 1)]
    if (n.levels > 1) {
        out2[, 2] <- rep(c(levels(group), "all"), times = n.var)
        xtab1 <- xtable(out2, align = paste("lll", align.disp.cols, 
            sep = ""), caption = cap, label = lab)
        xtab2 <- print(xtab1, include.rownames = FALSE, floating = FALSE, 
            type = "latex", hline.after = hlines, size = "footnotesize", 
            sanitize.text.function = function(x) {
                x
            }, tabular.environment = "longtable")
    }
    if (n.levels == 1) {
        out3 <- out2[(1:n.var - 1) * 2 + 1, -2]
        xtab1 <- xtable(out3, align = paste("ll", align.disp.cols, 
            sep = ""), caption = cap, label = lab)
        xtab2 <- print(xtab1, include.rownames = FALSE, floating = FALSE, 
            type = "latex", size = "footnotesize", sanitize.text.function = function(x) {
                x
            }, tabular.environment = "longtable")
    }
}
