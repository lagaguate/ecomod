fishingM <- function(u,M) {
#// xploitation rate, u, and natural mortality, M, calculate fishing mortality FM, given fish are dieing at at rate M during the same period U is occurring

  func <- function(Fi,u,M) { u - (Fi/(Fi+M))*(1-exp(-(Fi+M))) }

  FishMort <- rep(NA,length(u))
  attributes(FishMort) <- attributes(u)

  for (i in 1:length(u)) {
    uvalue <- u[i]
    if (!is.na(uvalue)) {
      if (uvalue==0) {
        FishMort[i] <- 0
      } else {
        ur <- uniroot(func,lower=0,upper=10,u=uvalue,M=M)
 #       if (ur$message!="normal termination") stop(ur$message)
        FishMort[i] <- ur$root
      }
    }  
  }
  return(FishMort)  
}

