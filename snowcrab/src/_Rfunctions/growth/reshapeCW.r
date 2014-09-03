reshapeCW <- function(x,nsamp=50) {
        yrs <- sort(unique(x$yr))
        out<- c()
        for(i in 1:length(yrs)) {
        if(i==1) { 
        out <- x[x$yr==yrs[i],'cw']
        } else {
        out <- cbindPad(out,x[x$yr==yrs[i],'cw'])
                }
        }
        j <- apply(out, 2, function(x) sum(!is.na(x)))
        if(any(j<nsamp)) {
        j <- which(j<nsamp)
        out <- out[,-j]
        yrs <- yrs[-j]
        }
                return(list(out,yrs))

}