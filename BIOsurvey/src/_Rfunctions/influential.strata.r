influential.strata <- function (object, Plot = TRUE, ...) {
    if (!inherits(object, "strata")) 
        stop("Not a legitimate strata object")
    yh <- as.vector(sapply(object$yhi, mean))
    yst <- sum(object$Wh * yh, na.rm = TRUE)
    str.list <- unique(object$Strata)
    max.str <- length(str.list)
    yinfl <- vector("list", max.str)
    for (i in 1:max.str) {
        yinfl[[i]] <- ((object$yhi[[i]] * object$Wh[i])/object$nh[i])/yst
    }
    res <- list(x = unlist(object$yhi), y = as.vector(unlist(yinfl)), 
        ylabs = rep(object$Strata, object$nh))
    if (Plot) {
        plot(res, ylab = "Proportion of Stratified Mean", ...)
        title(main = "Influence plot for Survey Data")
        invisible(res)
    }
    else {
        oldClass(res) <- "influential.plot"
        res
    }
}
