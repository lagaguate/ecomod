print.strata <- function (x, ...) {
    out.table <- matrix(nrow = length(x$Wh), ncol = 7, dimnames = list(NULL, 
        c("Strata", "Sets", "Wh", "Mean", "Std. Err.", "RE(%)","Sets.w.Spec")))
    out.table[, 1] <- sort(unique(x$Strata))
    out.table[, 2] <- x$nh
    out.table[, 3] <- x$Wh
    out.table[, 4] <- as.vector(sapply(x$yhi, mean))
    out.table[, 5] <- sqrt(as.vector(sapply(x$yhi, var))/x$nh)
    out.table[, 6] <- 'Not.Est'#(100 * out.table[, 5])/out.table[, 4] #turned off relative efficiency
    out.table[, 7] <- x$nhws
    options(digits = 4)
    print(out.table)
    invisible(out.table)
}
