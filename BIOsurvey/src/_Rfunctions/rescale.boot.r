 rescale.boot <- function (X, nred) {
    n <- length(X)
    if (length(X) == nred) {
        ans <- X
    }
    else {
        ans <- sample(X, size = n - nred, replace = TRUE)
        mh <- length(X) - nred
        nh <- length(X)
        yh <- mean(X)
        ans <- yh + ((mh^0.5) * (nh - 1)^(-0.5) * (ans - yh))
    }
    ans
}
