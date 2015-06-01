ECDF.strata <- function (object, Plot = TRUE, ...) {
    if (!inherits(object, "strata")) 
        stop("Not a legitimate strata object")
    Whi <- vector("list", max.str <- length(object$yhi))
    wh <- object$Wh/object$nh
    for (i in 1:max.str) {
        Whi[[i]] <- rep(wh[i], object$nh[i])
    }
    yhi <- as.vector(unlist(object$yhi))
    Whi <- as.vector(unlist(Whi))
    yhi.ord <- order(yhi)
    results <- list(x = yhi[yhi.ord], y = cumsum(Whi[yhi.ord]))
    if (Plot) {
        ECDF.strata.plot(results, ...)
        title(main = "Empirical Cumulative Distribution")
        invisible(results)
    }
    else results
}
