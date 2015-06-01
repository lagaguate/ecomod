boot.strata <- function (data, nless = 0, nresamp = 1, method = c("RESCALE",   "BWR", "NAIVE")) {
    if (!inherits(data, "strata")) 
        stop("Not a legitimate strata object, need to run Stratify() first")
    call <- match.call(expand = FALSE)
    method <- match.arg(method)
    res1 <- data
    res3 <- summary(res1)
    if (nresamp == 0) 
        stop("No resampling done.")
    out <- matrix(0, nrow = nresamp + 1, ncol = 2, dimnames = list(c("Actual", 
        1:nresamp), c("Mean", "Variance")))
    out[1, ] <- c(res3$yst, (res3$se.yst)^2)
    if (method == "RESCALE") {
        res1$nh <- res1$nh - nless
        for (i in 1:nresamp) {
            yhib <- sapply(res1$yhi, rescale.boot, nless)
            out[i + 1, ] <- c(sum(res1$Wh * as.vector(sapply(yhib, 
                mean)), na.rm = TRUE), (sum((((res1$Nh * (res1$Nh - 
                res1$nh))/sum(res1$Nh)^2) * (as.vector(sapply(yhib, 
                var))))/res1$nh, na.rm = TRUE)))
        }
    }
    if (method == "BWR") {
        fh <- res1$nh/res1$Nh
        kh <- (res1$nh - 1)/(1 - fh)
        ph <- ((1/kh) - (1/ceiling(kh)))/((1/floor(kh)) - (1/ceiling(kh)))
        for (i in 1:nresamp) {
            yhib <- bwr.boot(res1$yhi, kh, ph, sample, replace = TRUE, 
                simplify = FALSE)
            yhib[res1$nh == 1] <- res1$yhi[res1$nh == 1]
            out[i + 1, ] <- c(sum(res1$Wh * as.vector(sapply(yhib, 
                mean)), na.rm = TRUE), (sum((((res1$Nh * (res1$Nh - 
                as.vector(sapply(yhib, length))))/sum(res1$Nh)^2) * 
                (as.vector(sapply(yhib, var))))/as.vector(sapply(yhib, 
                length)), na.rm = TRUE)))
        }
    }
    if (method == "NAIVE") {
        for (i in 1:nresamp) {
            yhib <- sapply(res1$yhi, sample, replace = TRUE, 
                simplify = FALSE)
            out[i + 1, ] <- c(sum(res1$Wh * as.vector(sapply(yhib, 
                mean)), na.rm = TRUE), (sum((((res1$Nh * (res1$Nh - 
                as.vector(sapply(yhib, length))))/sum(res1$Nh)^2) * 
                (as.vector(sapply(yhib, var))))/as.vector(sapply(yhib, 
                length)), na.rm = TRUE)))
        }
    }
    res <- list(orig.mean = out[1, 1], orig.var = out[1, 2], 
        boot.means = out[c(2:(nresamp + 1)), 1], boot.vars = out[c(2:(nresamp + 
            1)), 2], accel = accel.str(res1), call = call, method = method)
    oldClass(res) <- "boot"
    return(res)
}
