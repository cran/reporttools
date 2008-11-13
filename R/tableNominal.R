`tableNominal` <-
function (vars, nams, group = NA, subset = NA, miss.cat = NA, 
    print.pval = c("none", "fisher", "chi2")[1], vertical = TRUE, 
    cap = "", lab = "") 
{
    vert.lin <- "|"
    if (vertical == FALSE) {
        vert.lin <- ""
    }
    for (i in 1:length(nams)) {
        nams[i] <- gsub("_", "\\\\_", as.character(nams[i]))
    }
    if (max(is.na(miss.cat)) == 0) {
        for (i in miss.cat) {
            vars[[i]] <- NAtoCategory(vars[[i]], label = "missing")
        }
    }
    if (max(is.na(subset) == FALSE) == 1) {
        group <- group[subset]
        for (v in 1:length(vars)) {
            vars[[v]] <- vars[[v]][subset]
        }
    }
    vars <- lapply(vars, factor)
    if (max(is.na(group) == TRUE) == 1) {
        group <- rep(1, length(vars[[1]]))
    }
    group <- as.factor(group)
    ns.level <- unlist(lapply(lapply(vars, levels), length))
    n.group <- length(levels(group))
    n.var <- length(nams)
    out <- matrix(NA, ncol = 2 + 3 * (n.group + 1), nrow = (sum(ns.level) + 
        n.var))
    out <- data.frame(out)
    for (i in 1:n.var) {
        ind <- max(cumsum(ns.level[1:i])) - ns.level[i] + 1:(ns.level[i] + 
            1) + (i - 1)
        splits <- split(vars[[i]], group)
        for (g in 1:n.group) {
            tmp <- splits[[g]]
            tmp <- tmp[is.na(tmp) == FALSE]
            if (sum(is.na(tmp)) > 0) {
                excl <- NULL
            }
            else {
                excl <- NA
            }
            tab <- table(tmp, exclude = excl)
            tab.s <- round(100 * tab/sum(tab), 2)
            out[ind, 2 + 3 * (g - 1) + 1] <- c(tab, sum(tab))
            out[ind, 2 + 3 * (g - 1) + 2] <- c(tab.s, sum(tab.s))
            out[ind, 2 + 3 * (g - 1) + 3] <- c(cumsum(tab.s), 
                NA)
        }
        out[ind[1], 1] <- nams[[i]]
        out[ind, 2] <- c(levels(vars[[i]]), "all")
        tab2 <- table(vars[[i]])
        tab2.s <- round(100 * tab2/sum(tab2), 2)
        out[ind, 2 + 3 * n.group + 1] <- c(tab2, sum(tab2))
        out[ind, 2 + 3 * n.group + 2] <- c(tab2.s, sum(tab2.s))
        out[ind, 2 + 3 * n.group + 3] <- c(cumsum(tab2.s), NA)
        ind1 <- n.group > 1
        ind2 <- print.pval %in% c("fisher", "chi2")
        ind3 <- length(levels(vars[[i]])) > 1
        ind4 <- 1 - max(unlist(lapply(lapply(splits, is.na), 
            sum)) == unlist(lapply(lapply(splits, is.na), length)))
        if (ind1 * ind2 * ind3 * ind4 == 1) {
            if (print.pval == "fisher") {
                pval <- fisher.test(vars[[i]], group, simulate.p.value = TRUE)$p.value
            }
            if (print.pval == "chi2") {
                pval <- chisq.test(vars[[i]], group, correct = TRUE)$p.value
            }
            out[max(ind), 1] <- paste("p = ", format.pval(pval), 
                sep = "")
        }
    }
    col.tit <- c("$n", "$\\%", "$\\sum \\%")
    dimnames(out)[[2]] <- c("{\\bf Variable}", "{\\bf Levels}", 
        paste(col.tit, "_{", rep(c(levels(group), "\\mathrm{all}"), 
            each = 3), "}$", sep = ""))
    al <- paste("lll", vert.lin, "rrr", sep = "")
    if (n.group > 1) {
        for (i in 1:n.group) {
            al <- paste(al, vert.lin, "rrr", sep = "")
        }
    }
    tmp <- cumsum(ns.level + 1)
    hlines <- sort(c(0, tmp - 1, rep(tmp, each = 2)))
    if (n.group > 1) {
        xtab1 <- xtable(out, digits = c(rep(0, 3), rep(c(0, 1, 
            1), n.group + 1)), align = al, caption = cap, label = lab)
        xtab2 <- print(xtab1, include.rownames = FALSE, floating = FALSE, 
            type = "latex", hline.after = hlines, size = "footnotesize", 
            sanitize.text.function = function(x) {
                x
            }, tabular.environment = "longtable")
    }
    if (n.group == 1) {
        xtab1 <- xtable(out[, 1:5], digits = c(rep(0, 3), c(0, 
            1, 1)), align = al, caption = cap, label = lab)
        xtab2 <- print(xtab1, include.rownames = FALSE, floating = FALSE, 
            type = "latex", hline.after = hlines, size = "footnotesize", 
            sanitize.text.function = function(x) {
                x
            }, tabular.environment = "longtable")
    }
}
