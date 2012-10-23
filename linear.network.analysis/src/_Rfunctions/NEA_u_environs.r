
  NEA_u_environs = function(TFAnal, StAnal) {

# This subfunction calculates the unit environs (input, output,
# throughflow, and storage) for the given system.  Noticeable numerical
# error is usually apparent in the resultant matricies. Here, I use the
# subfunction "environ_error" removes an arbitrary amount of error by
# setting values less than "environ_error_tol" to 0.  A more appropriate way
# might be to round the values to a particular decimal place.

# IMPORTANT:  Check the error tolerance level to make sure it is
# appropriate

    G = TFAnal$G
    GP = TFAnal$GP
    N = TFAnal$N
    NP = TFAnal$NP

    P = StAnal$P
    PP = StAnal$PP
    Q = StAnal$Q
    QP = StAnal$QP
   
    n = nrow(G)
    I = diag(n)
 

    E  = EP = SE = SEP = array(data=0, dim=c(n+1,n+1,n) )
    environ_error_tol=1e-10 
    # The value of this is arbitrary. Other ways to set this variable are possible.

    i1n = c(1:n)
    # Throughflow unit environs ----------------
    for (i in i1n) {
      E[i1n,i1n,i] = G %*% diag(N[,i]) - diag(N[,i]) 
      E[n+1,i1n,i] = colSums( -E[i1n,i1n,i] )
      E[i1n,n+1,i] = rowSums( -E[i1n,i1n,i] )		# unit output flow environs
    }
    E[ abs(E) < environ_error_tol ] = 0

    for (i in i1n ) {
      EP[i1n,i1n,i] = diag(NP[i,]) %*% GP - diag(NP[i,]) 
      EP[n+1,i1n,i] = colSums( -EP[i1n,i1n,i] )   # unit input flow environs
      EP[i1n,n+1,i] = rowSums( -EP[i1n,i1n,i] )		
    }
    EP[ abs(EP) < environ_error_tol ] = 0


    # Storage unit environs	--------------------
    for (i in i1n) {
      SE[i1n,i1n,i] = P %*% diag(Q[,i]) - diag(Q[,i]) 
      SE[n+1,i1n,i] = colSums( -SE[i1n,i1n,i] )
      SE[i1n,n+1,i] = rowSums( -SE[i1n,i1n,i] )		# unit output storage environs
    }
    SE[ abs(SE) < environ_error_tol ] = 0

    for (i in i1n ) {
      SEP[i1n,i1n,i] = diag(QP[i,]) %*% PP - diag(QP[i,]) 
      SEP[n+1,i1n,i] = colSums( -SEP[i1n,i1n,i] )   # unit input storage environs
      SEP[i1n,n+1,i] = rowSums( -SEP[i1n,i1n,i] )		
    }
    SEP[ abs(SEP) < environ_error_tol ] = 0

    res = list(E=E, EP=EP, SE=SE, SEP=SEP)
    return( res )
  }


