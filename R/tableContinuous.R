`tableContinuous` <-
function (vars, nams, group = NA, subset = NA, disp.cols = c("n", 
    "min", "q1", "median", "mean", "q3", "max", "s", "iqr", "na"), 
    prec = 1, col.tit = NA, print.pval = c("none", "anova", "kruskal")[1], 
    cap = "", lab = "") 
{
    dc <- c("n", "min", "q1", "median", "mean", "q3", "max", 
        "s", "iqr", "na")
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
    if (max(is.na(col.tit) == TRUE) == 1) {
        col.tit <- c("{\\bf Variable}", "{\\bf Levels}", "n", 
            "Min", "$q_1$", "$\\widetilde{x}$", "$\\bar{x}$", 
            "$q_3$", "Max", "$s$", "IQR", "\\#NA")
    }
    n.levels <- 1
    if (max(is.na(group) == 1) == 0) {
        group <- as.factor(group)
        n.levels <- length(levels(group))
    }
    n.var <- length(nams)
    out <- matrix(NA, ncol = 12, nrow = (n.levels + 1) * n.var)
    out <- data.frame(out)
    if (n.levels == 1) {
        prec <- c(rep(0, 3), rep(prec, 8), 0)
        ali <- "ll"
    }
    if (n.levels > 1) {
        prec <- c(rep(0, 3), rep(prec, 9), 0)
        ali <- "lll"
    }
    for (i in 1:n.var) {
        ind <- (i - 1) * (n.levels + 1) + 1:(n.levels + 1)
        splits <- list(vars[[i]])
        if (max(is.na(group) == 1) == 0) {
            splits <- split(vars[[i]], group)
        }
        for (j in 1:n.levels) {
            tmp <- splits[[j]]
            if (sum(is.na(tmp) == FALSE) != 0) {
                out[ind[j], 3] <- sum(is.na(tmp) == FALSE)
                out[ind[j], 4:9] <- summary(tmp)[1:6]
                out[ind[j], 10] <- sd(tmp, na.rm = TRUE)
                out[ind[j], 11] <- out[ind[j], 8] - out[ind[j], 
                  5]
                out[ind[j], 12] <- sum(is.na(tmp) == TRUE)
            }
        }
        out[max(ind), 3] <- sum(is.na(vars[[i]]) == FALSE)
        out[max(ind), 4:9] <- summary(vars[[i]])[1:6]
        out[max(ind), 10] <- sd(vars[[i]], na.rm = TRUE)
        out[max(ind), 11] <- out[max(ind), 8] - out[max(ind), 
            5]
        out[max(ind), 12] <- sum(is.na(vars[[i]]) == TRUE)
        ind1 <- n.levels > 1
        ind2 <- print.pval %in% c("anova", "kruskal")
        ind3 <- min(unlist(lapply(splits, sum, na.rm = TRUE))) > 
            0
        if (ind1 * ind2 * ind3 == 1) {
            if (print.pval == "anova") {
                pval <- anova(lm(vars[[i]] ~ group))$"Pr(>F)"[1]
            }
            if (print.pval == "kruskal") {
                pval <- kruskal.test(splits)$p.value
            }
            out[(i - 1) * (n.levels + 1) + n.levels + 1, 1] <- paste("p = ", 
                format.pval(pval), sep = "")
        }
    }
    align.disp.cols <- ""
    disp.cols2 <- c(1:2, 2 + disp.cols.num)
    for (i in 1:length(disp.cols.num)) {
        align.disp.cols <- paste(align.disp.cols, "r", sep = "")
    }
    out2 <- out[, disp.cols2]
    out2[((1:n.var) - 1) * (n.levels + 1) + 1, 1] <- nams
    dimnames(out2)[[2]] <- col.tit[disp.cols2]
    tmp <- cumsum(rep(n.levels, n.var) + 1)
    hlines <- sort(c(0, tmp - 1, rep(tmp, 2)))
    if (n.levels > 1) {
        out2[, 2] <- rep(c(levels(group), "all"), times = n.var)
        xtab1 <- xtable(out2, digits = prec[c(1, 1 + disp.cols2)], 
            align = paste(ali, align.disp.cols, sep = ""), caption = cap, 
            label = lab)
        xtab2 <- print(xtab1, include.rownames = FALSE, floating = FALSE, 
            type = "latex", hline.after = hlines, size = "footnotesize", 
            sanitize.text.function = function(x) {
                x
            }, tabular.environment = "longtable")
    }
    if (n.levels == 1) {
        hlines <- c(-1, 0)
        out3 <- out2[(1:n.var - 1) * 2 + 1, -2]
        xtab3 <- xtable(out3, digits = prec[disp.cols2], align = paste(ali, 
            align.disp.cols, sep = ""), caption = cap, label = lab)
        xtab4 <- print(xtab3, include.rownames = FALSE, floating = FALSE, 
            type = "latex", hline.after = hlines, size = "footnotesize", 
            sanitize.text.function = function(x) {
                x
            }, tabular.environment = "longtable")
    }
}
