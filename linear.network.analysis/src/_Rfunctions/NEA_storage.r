      
  NEA_storage = function(F,x) { 
    # Input and output oriented storage normalized environ analysis
    
    n = nrow(F)
    I = diag(n)
    T = colSums( t(F) ) + z

    # Direct storage matrices
    FD = F - diag(T)				# flow matrix with negative throughflows on diagonal
    C = FD %*% solve(diag(x))   # fij/xj for i,j=1:n -- output matrix
    CP = solve(diag(x)) %*% FD  # fij/xi for i,j=1:n -- input matrix
    dt = -1 / floor(min(diag(C)))	# smallest whole number to make diag(C) nonnegative
    P  = I + C * dt               # non-dimensional direct output storage matrix
    PP = I + CP * dt              # non-dimensional direct input storage matrix
    
    # Integral storage matrices
    S  = -solve(C)		    # dimensionalized integral output community matrix
    SP = -solve(CP)	    	# dimensionalized integral input community matrix
    Q  = solve(I-P)		    # integral output storage matrix -- I+P+P^2+P^3+...
    QP = solve(I-PP)	    # integral input storage matrix -- I+PP+PP^2+PP^3+...
    dQ = diag(Q)		    	# diag of integral output storage matrix (=diag(QP))

    # Storage environ properties
    p = rep(1,n)
    TSTcs = sum(((dQ-p)/dQ)*T) 	# cycled (mode 2) throughflow
    TSTs  = sum(T) 		    	  # total system throughflow
    CIS   = TSTcs/TSTs 		    	# cycling index (storage)

    # Amplification parameter
    NAS  = length( which ((Q-diag(diag(Q)))>1))  
    NASP = length(which ((QP-diag(diag(QP)))>1))

    # Indirect effects parameter
    IDS  = sum(Q-I-P)/sum(P) 	    # indirect to direct ratio (output matrix)
    IDSP = sum(QP-I-PP)/sum(PP)   # indirect to direct ratio (input matrix)	

    # Homogenization parameter
    CVP =sd(as.vector(P))/mean(P)	    	# Coefficient  of variation for G
    CVQ =sd(as.vector(Q))/mean(Q)   	    # Coefficient  of variation for N
    HS = CVP/CVQ				    	# homogenization parameter (output storage)

    CVPP = sd(as.vector(PP))/mean(PP)		# Coefficient  of variation for GP
    CVQP = sd(as.vector(QP))/mean(QP)		# Coefficient  of variation for NP
    HSP  = CVPP/CVQP			    	# homogenization parameter (input storage)

    stor_ep = list( CIS=CIS, NAS=NAS, NASP=NASP, IDS=IDS, IDSP=IDSP, HS=HS, HSP=HSP, P=P, PP=PP,Q=Q, QP=QP )

    return (stor_ep)

  }


