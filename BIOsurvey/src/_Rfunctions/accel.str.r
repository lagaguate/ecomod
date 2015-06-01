accel.str <- function (x.str) {
	#for BCa conf.intervals
    if (any(x.str$nh <= 1)) {
        return(NA)
    }
    else {
        temp <- accel.est(x.str)
        fh <- x.str$nh/x.str$Nh
        accel <- sum(x.str$Wh^3 * (1 - fh) * (1 - 2 * fh) * temp[1, 
            ])
        acceld <- sum((x.str$Nh * (x.str$Nh - x.str$nh) * temp[2, 
            ])/sum(x.str$Nh)^2)
        accel/(6 * acceld^(3/2))
    }
}
