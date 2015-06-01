qqnorm.boot <- function (y, ylab = "", ...) {
    qqnorm(y$boot.means, ylab = ylab)
    invisible(qqline(y$boot.means))
}
