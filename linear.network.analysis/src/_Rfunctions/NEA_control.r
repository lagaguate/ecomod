
  NEA_control = function( TFAnal, StAnal ) {
    # compute ratio control or dominance matrix
   
    N = TFAnal$N
    NP = TFAnal$NP

    Q = StAnal$Q
    QP = StAnal$QP
   
    n = nrow(N)
    I = diag(n)

    CN = CQ = array(data=0, dim=c(n,n) )

    # Throughflow
    CN_diff = N / t(NP) 
    j = which(CN_diff < 1 & CN_diff>=0) 
    CN[j] = 1 - CN_diff[j]

    # Storage
    CQ_diff = Q / t(QP) 
    i = which(CQ_diff < 1 & CQ_diff >= 0) 
    CQ[j] = 1 - CQ_diff[j]

    res = list(CN=CN, CQ=CQ, CN_diff=CN_diff, CQ_diff=CQ_diff)
    return(res)

  }


