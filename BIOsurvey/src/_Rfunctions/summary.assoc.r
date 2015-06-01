summary.assoc <- function (object, ...) {
    options(digits = 4)
    cat("\n", object$prefer.lab, "\n", "Test Statistic=", format(object$test.stat), 
        "\n", "\n", object$prefer2.lab, "\n", object$prefer3.lab, 
        " for randomization test=", format((sum(object$test.stat <= 
            object$test.stat.rep))/(length(object$test.stat.rep))), 
        "\n")
    cat("\n", "Summary of distribution of", length(object$test.stat.rep), 
        "test statistics", "\n", "from randomization simulation.", 
        "\n", "\n")
    summary(unclass(object$test.stat.rep))
}
