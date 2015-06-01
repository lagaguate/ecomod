summary.strata <- function (object, alpha.t = 0.05, effic = FALSE, nopt = FALSE, ...) {
    if (!inherits(object, "strata")) 
        stop("Not a legitimate strata object")
    yh <- as.vector(sapply(object$yhi, mean))
    yst <- sum(object$Wh * yh, na.rm = TRUE)
    sh <- as.vector(sapply(object$yhi, var))
    se.yst <- sqrt(sum((((object$Nh * (object$Nh - object$nh))/sum(object$Nh)^2) * 
        sh)/object$nh, na.rm = TRUE))
    ah <- (object$Nh * (object$Nh - object$nh))/object$nh
    df.yst <- (sum(ah * sh, na.rm = TRUE)^2)/(sum(((ah * sh)^2)/(object$nh - 
        1), na.rm = TRUE))
    ci.yst <- yst + (c(qt(alpha.t/2, df.yst), -qt(alpha.t/2, 
        df.yst)) * se.yst)
    if(any(length(object$Wh) != length(object$nhws) | length(object$nhws)!= length(object$nh))) browser()
    dwao <- sum(object$Wh*(object$nhws / object$nh)) * sum(object$Nh) * 0.011801 
    res <- list(yst = yst, se.yst = se.yst, Yst = yst * sum(object$Nh), 
        df.yst = df.yst, alpha = alpha.t, ci.yst = ci.yst, dwao = dwao)
    if (effic == TRUE) {
        N <- sum(object$Nh)
        n <- sum(object$nh)
        vran <- ((N - n)/(n * N)) * sum(object$Wh * sh, na.rm = TRUE)
        effic.str <- ((N - n)/(n * (N - 1))) * (sum(object$Wh * 
            (yh - yst)^2) - sum((object$Wh * (1 - object$Wh) * 
            sh)/object$nh, na.rm = TRUE))
        vran <- vran + effic.str
        effic.alloc <- sum(((1/n) - (object$Wh/object$nh)) * 
            object$Wh * sh, na.rm = TRUE)
        effic.str <- (100 * effic.str)/vran
        effic.alloc <- (100 * effic.alloc)/vran
        min.var <- ((1/n) * sum(object$Wh * sqrt(sh), na.rm = TRUE)^2) - 
            sum(object$Wh * sh, na.rm = TRUE)/N
        res <- c(res, list(effic.alloc = effic.alloc, effic.str = effic.str, 
            var.ran = vran, max.eff = (100 * (vran - min.var))/vran, 
            min.var = min.var))
    }
    if (nopt) {
        n.opt.out <- as.data.frame(matrix(NA, length(yh) + 1, 
            6))
        names(n.opt.out) <- c("Strata", "Observed", "Optimal", 
            "Perc.Increase.Var.opt", "Compromise", "Perc.Increase.Var.comp")
        n.opt.out$Strata <- c(object$Strata, "Total")
        n.opt.out$Observed <- c(object$nh, n.tot <- sum(object$nh))
        if (all(object$nh != 1)) {
            n.opt <- (((sqrt(sapply(object$yhi, var)) * object$Wh)/sum(sqrt(sapply(object$yhi, 
                var)) * object$Wh, na.rm = TRUE)) * sum(object$nh))
            n.opt <- round(n.opt)
            if (n.tot > sum(n.opt)) {
                n.opt[n.opt == max(n.opt)] <- n.opt[n.opt == 
                  max(n.opt)] + (n.tot - sum(n.opt))
            }
            if (n.tot < sum(n.opt)) {
                n.opt[n.opt == max(n.opt)] <- n.opt[n.opt == 
                  max(n.opt)] - (n.tot - sum(n.opt))
            }
            n.opt.out$Optimal <- c(n.opt, sum(n.opt))
            perc.increase <- (100 * (((object$nh - n.opt)^2)/object$nh))/sum(object$nh)
            n.opt.out$Perc.Increase.Var.opt <- c(perc.increase, 
                sum(perc.increase))
            n.comp.ind <- (n.opt <= 1)
            n.comp.opt <- sum(n.comp.ind)
            n.opt2 <- n.opt
            n.opt2[n.comp.ind] <- 2
            n.opt2[n.opt2 == max(n.opt2)] <- n.opt2[n.opt2 == 
                max(n.opt2)] - (sum(n.opt2) - sum(n.opt))
            n.opt.out$Compromise <- c(n.opt2, sum(n.opt2))
            Perc.Increase.Var.comp <- (100 * (((n.opt2 - n.opt)^2)/n.opt2))/sum(object$nh)
            n.opt.out$Perc.Increase.Var.comp <- c(Perc.Increase.Var.comp, 
                sum(Perc.Increase.Var.comp))
        }
        res <- c(res, list(n.opt = n.opt.out))
    }
    options(digits = max(options()$digits - 5, 5))
    c(res, list(descrip = "Stratified Analysis"))
}

