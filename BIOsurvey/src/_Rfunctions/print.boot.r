print.boot <- function (x, ...) {
    options(digits = 4)
    cat("\n", "Original Mean=", format(x$orig.mean), "\n", "Original Variance", 
        format(x$orig.var), "\n", "Number of bootstraps = ", 
        length(x$boot.means), "\n", "Bootstrap Mean=", format(mean(x$boot.means)), 
        "\n", "Average Bootstrap Variance", format(mean(x$boot.vars)), 
        "\n")
}
