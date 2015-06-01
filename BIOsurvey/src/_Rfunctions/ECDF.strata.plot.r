ECDF.strata.plot <- function (obj, ...) {
    plot(obj, ylab = "Cumulative Frequency", ylim = c(0, 1), 
        type = "b", ...)
}
