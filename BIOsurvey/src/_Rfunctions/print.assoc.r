print.assoc <- function (x, ...) {
    options(digits = 4)
    cat("\n", x$prefer.lab, "\n", "Test Statistic=", format(x$test.stat), 
        "\n", "\n", x$prefer2.lab, "\n", x$prefer3.lab, "for randomization test=", 
        format((sum(x$test.stat <= x$test.stat.rep))/(length(x$test.stat.rep))), 
        "\n", "Number of randomizations = ", length(x$test.stat.rep), 
        "\n")
}
