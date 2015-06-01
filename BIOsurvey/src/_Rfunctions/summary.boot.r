summary.boot <- function (object, CI.method = c("Percentile", "BC", "BCa"), alpha.b = 0.05,prints=F,     ...) {
    if (!inherits(object, "boot")) 
        stop("Not a legitimate boot object, need to run boot.strat() first")
    options(digits = 4)
    CI.method <- match.arg(CI.method)
    boot.est <- mean(object$boot.means)
    if (CI.method == "Percentile") {
        ci.boot <- quantile(object$boot.means, probs = c(alpha.b/2, 
            (1 - alpha.b/2), 0.5))
    }
    if (CI.method == "BC") {
        loc.bc <- sum(object$boot.means < boot.est)
        lim.bc <- sort(object$boot.means)[c(loc.bc, loc.bc + 
            1)]
        z0 <- (loc.bc + ((boot.est - lim.bc[1])/(lim.bc[2] - 
            lim.bc[1])))
        z0 <- qnorm(z0/length(object$boot.means))
        probs.z0 <- pnorm(qnorm(c(alpha.b/2, (1 - alpha.b/2), 
            0.5)) + 2 * z0)
        ci.boot <- quantile(object$boot.means, probs = probs.z0)
    }
    if (CI.method == "BCa") {
        if (is.na(object$accel)) 
            stop("One or more strata have 1 or less observations.\nNo acceleration estimate possibl\ne.")
        loc.bc <- sum(object$boot.means < boot.est)
        lim.bc <- sort(object$boot.means)[c(loc.bc, loc.bc + 
            1)]
        z0 <- (loc.bc + ((boot.est - lim.bc[1])/(lim.bc[2] - 
            lim.bc[1])))
        z0 <- qnorm(z0/length(object$boot.means))
        a1 <- pnorm(z0 + (z0 + qnorm(alpha.b/2))/(1 - object$accel * 
            (z0 + qnorm(alpha.b/2))))
        a2 <- pnorm(z0 + (z0 + qnorm(1 - alpha.b/2))/(1 - object$accel * 
            (z0 + qnorm(1 - alpha.b/2))))
        a3 <- pnorm(z0 + (z0 + qnorm(0.5))/(1 - object$accel * 
            (z0 + qnorm(0.5))))
        ci.boot <- quantile(object$boot.means, probs = c(a1, 
            a2, a3))
    }
   if(prints) {cat("\n", "Original Mean =", format(object$orig.mean), "\n", 
        "Original Variance =", format(object$orig.var), "\n", 
        "Number of bootstraps = ", length(object$boot.means), 
        "\n", "Bootstrap Mean=", format(boot.est), "\n", "Variance of Bootstrap Mean=", 
        format(var(object$boot.means)), "\n", c(CI.method, "CI's for alpha="), 
        alpha.b, "are ", format(ci.boot[1:2]), "\n", "Length =", 
        format(ci.boot[2] - ci.boot[1]), "\n", "Shape=", format(log((ci.boot[2] - 
            ci.boot[3])/(ci.boot[3] - ci.boot[1]))), "\n", "Method = ", 
        object$method, "\n")
   }
    #summary(object$boot.means, digits = 4) old output
    return(ci.boot)
}

