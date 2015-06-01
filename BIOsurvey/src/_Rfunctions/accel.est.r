accel.est <- function (data) {
	#for BCa conf.intervals
    no.strata <- length(unique(data$Strata))
    accel <- matrix(NA, 2, no.strata)
    for (j in 1:no.strata) {
        u <- rep(NA, data$nh[j])
        for (i in 1:data$nh[j]) {
            u[i] <- mean(data$yhi[[j]][-i])
        }
        accel[1, j] <- sum((mean(u) - u)^3)
        accel[2, j] <- sum((mean(u) - u)^2)
    }
    return(accel)
}
