pairwise.fisher.test <- function(x, g, p.adjust.method, ...){
     DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
     g <- factor(g)
     
     compare.levels <- function(i, j) {
          ind <- (as.integer(g) %in% c(i, j)) & (is.na(x) == FALSE) & (is.na(g) == FALSE)
          xi <- factor(x[ind], exclude = NULL)
          xj <- factor(g[ind], exclude = NULL)
          tab <- table(xi, xj)
          nonzeromarginal <- (min(apply(tab, 1, sum)) * min(apply(tab, 2, sum)) > 0)
          size <- ((nrow(tab) > 1) * (ncol(tab) > 1) > 0)
          if ((nonzeromarginal == TRUE) & (size == TRUE)){fisher.test(xi, xj, ...)$p.value} else {NA}
     }
     
     PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
     ans <- list(data.name = DNAME, p.value = PVAL, p.adjust.method = p.adjust.method)
     class(ans) <- "pairwise.htest"
     return(ans)
}
