bwr.boot <- function (X, kh, ph, FUN, ..., simplify = TRUE) {
    if (is.character(FUN)) 
        FUN <- get(FUN, mode = "function")
    else if (mode(FUN) != "function") {
        farg <- substitute(FUN)
        if (mode(farg) == "name") 
            FUN <- get(farg, mode = "function")
        else stop(paste("\"", farg, "\" is not a function", sep = ""))
    }
    class(X) <- NULL
    answer <- vector("list", length(X))
    nms <- names(X)
    if (is.recursive(X)) 
        names(X) <- NULL
    n <- length(X)
    all.same <- integer(n)
    for (i in seq(length = n)) {
        if (is.na(ph[i])) {
            nsam <- 1
        }
        else {
            if (rbinom(1, 1, ph[i]) == 1) {
                nsam <- floor(kh[[i]])
            }
            else {
                nsam <- ceiling(kh[[i]])
            }
        }
        ans <- FUN(X[[i]], size = nsam, ...)
        answer[i] <- list(ans)
        all.same[i] <- length(ans)
    }
    names(answer) <- nms
    if (simplify && length(all.same <- unique(all.same)) == 1 && 
        all.same > 0) {
        if (all.same[1] == 1) 
            unlist(answer, recursive = FALSE)
        else array(unlist(answer, recursive = FALSE), c(all.same, 
            n), list(NULL, names(answer)))
    }
    else answer
}
