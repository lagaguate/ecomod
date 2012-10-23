
  flow.modes = function(N,z) {

    # This function partitions flow into five different modes.  
    # Mode 0 is the boundary input -- flow that reaches a compartment from across the system boundary.  
    # Mode 1 is internal first passage flow -- total internal
    # flow from compartment j to compartment i for the first time along all
    # available pathways (including cycles that do not touch i).  
    # Mode 2 is cycled flow -- total contribution that returns to a compartment after its
    # initial visit.  Modes 3 and 4 are dissipative equivalents to Modes 1 
    # and 0, respectively.
    
    n = nrow(N)
    I = diag(n)
  
    mode0 = diag( drop( I %*% z ))
    mode1 = solve(diag(diag(N))) %*% N %*% diag(z) - diag( drop( I %*% z) )
    mode2 = (diag(diag(N))-I) %*% solve(diag(diag(N))) %*% N %*% diag(z)
    TSC = colSums((diag(diag(N))-I) %*% solve(diag(diag(N))) %*% N %*% diag(z))
    T0 = sum(mode0)
    T1 = sum(mode1)
    T2 = sum(mode2)
    T3 =T1
    T4 =T0
    T = T0 + T1 + T2

    modes = list(T0=T0, T1=T1, T2=T2, T3=T3, T4=T4)
    return(modes)
  }


