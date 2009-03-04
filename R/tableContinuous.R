`tableContinuous` <-
function (vars, nams, group = NA, subset = NA, stats = c("n", 
    "min", "q1", "median", "mean", "q3", "max", "s", "iqr", "na"), 
    prec = 1, col.tit = NA, print.pval = c("none", "anova", "kruskal")[1], 
    declare.zero = 10^-10, cap = "", lab = "", disp.cols = NA) 
{
    if (identical(disp.cols, NA) == FALSE) {
        stats <- disp.cols
    }
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
        col.tit <- c("{\\bf Variable}", "{\\bf Levels}", "$n$", 
            "Min", "$q_1$", "$\\widetilde{x}$", "$\\bar{x}$", 
            "$q_3$", "Max", "$s$", "IQR", "\\#NA")
    }
    group <- factor(group, exclude = NULL)
    n.levels <- 1
    if (max(is.na(group) == 1) == 0) {
        group <- as.factor(group)
        n.levels <- length(levels(group))
    }
    ncols <- length(stats)
    s1 <- unlist(lapply(stats, is.character))
    s1 <- (1:ncols)[s1]
    s2 <- unlist(lapply(stats, is.function))
    s2 <- (1:ncols)[s2]
    n.var <- length(nams)
    out <- matrix(NA, ncol = 12, nrow = (n.levels + 1) * n.var)
    out <- data.frame(out)
    out.fct <- matrix(NA, ncol = length(s2), nrow = (n.levels + 
        1) * n.var)
    out.fct <- data.frame(out.fct)
    for (i in 1:n.var) {
        ind <- (i - 1) * (n.levels + 1) + 1:(n.levels + 1)
        splits <- list(vars[[i]])
        if (max(is.na(group) == 1) == 0) {
            splits <- split(vars[[i]], group)
        }
        for (j in 1:n.levels) {
            tmp <- as.vector(splits[[j]])
            if (sum(is.na(tmp) == FALSE) != 0) {
                out[ind[j], 3] <- sum(is.na(tmp) == FALSE)
                out[ind[j], 4:9] <- summary(tmp)[1:6]
                out[ind[j], 10] <- sd(tmp, na.rm = TRUE)
                out[ind[j], 11] <- out[ind[j], 8] - out[ind[j], 
                  5]
                out[ind[j], 12] <- sum(is.na(tmp) == TRUE)
                if (length(s2) > 0) {
                  for (f in 1:length(s2)) {
                    out.fct[ind[j], f] <- stats[[s2[f]]](tmp[is.na(tmp) == 
                      FALSE])
                  }
                }
            }
        }
        vi <- as.vector(vars[[i]])
        out[max(ind), 3] <- sum(is.na(vi) == FALSE)
        out[max(ind), 4:9] <- summary(vi)[1:6]
        out[max(ind), 10] <- sd(vi, na.rm = TRUE)
        out[max(ind), 11] <- out[max(ind), 8] - out[max(ind), 
            5]
        out[max(ind), 12] <- sum(is.na(vi) == TRUE)
        out[, 3:12][abs(out[, 3:12]) <= declare.zero] <- 0
        if (length(s2) > 0) {
            for (f in 1:length(s2)) {
                out.fct[max(ind), f] <- stats[[s2[f]]](vi[is.na(vi) == 
                  FALSE])
            }
            out.fct[abs(out.fct) <= declare.zero] <- 0
        }
        ind1 <- n.levels > 1
        ind2 <- print.pval %in% c("anova", "kruskal")
        ind3 <- 1
        for (s in 1:length(splits)) {
            if (sum(is.na(splits[[1]]) == TRUE) == length(splits[[1]])) {
                ind3 <- 0
            }
        }
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
    dc <- c("n", "min", "q1", "median", "mean", "q3", "max", 
        "s", "iqr", "na")
    stats.num <- pmatch(stats[s1], dc)
    align.stats <- ""
    stats2 <- c(2 + stats.num)
    out2 <- matrix(NA, ncol = 2 + length(s1) + length(s2), nrow = (n.levels + 
        1) * n.var)
    out2 <- data.frame(out2)
    out2[, c(1, 2, 2 + s1)] <- out[, c(1, 2, stats2)]
    out2[, 2 + s2] <- out.fct
    out2[((1:n.var) - 1) * (n.levels + 1) + 1, 1] <- nams
    dimnames(out2)[[2]][c(1:2, 2 + s1)] <- col.tit[c(1:2, stats2)]
    if (length(s2) > 0) {
        dimnames(out2)[[2]][2 + s2] <- names(stats)[names(stats) != 
            ""]
    }
    for (i in 1:ncols) {
        align.stats <- paste(align.stats, "r", sep = "")
    }
    if (n.levels == 1) {
        prec <- c(rep(0, 2), rep(prec, ncols))
        ali <- "ll"
        out2 <- out2[, -2]
    }
    if (n.levels > 1) {
        prec <- c(rep(0, 3), rep(prec, ncols))
        ali <- "lll"
    }
    prec.ind <- dimnames(out2)[[2]] %in% c("n", "\\#NA")
    prec.ind <- (1:length(prec.ind)) * prec.ind
    prec.ind <- prec.ind[prec.ind > 0] + 1
    prec[prec.ind] <- 0
    tmp <- cumsum(rep(n.levels, n.var) + 1)
    hlines <- sort(c(0, tmp - 1, rep(tmp, 2)))
    if (n.levels == 1) {
        hlines <- 0
        out3 <- out2[(1:n.var - 1) * 2 + 1, ]
        xtab3 <- xtable(out3, digits = prec, align = paste(ali, 
            align.stats, sep = ""), caption = cap, label = lab)
        xtab4 <- print(xtab3, include.rownames = FALSE, floating = FALSE, 
            type = "latex", hline.after = hlines, size = "footnotesize", 
            sanitize.text.function = function(x) {
                x
            }, tabular.environment = "longtable")
    }
    if (n.levels > 1) {
        out2[, 2] <- rep(c(levels(group), "all"), times = n.var)
        xtab1 <- xtable(out2, digits = prec, align = paste(ali, 
            align.stats, sep = ""), caption = cap, label = lab)
        xtab2 <- print(xtab1, include.rownames = FALSE, floating = FALSE, 
            type = "latex", hline.after = hlines, size = "footnotesize", 
            sanitize.text.function = function(x) {
                x
            }, tabular.environment = "longtable")
    }
}
