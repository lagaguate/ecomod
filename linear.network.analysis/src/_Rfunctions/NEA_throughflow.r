

  NEA_throughflow = function(F, y, z) {

    n = nrow(F)
    I = diag(n)
    T = colSums( t(F) ) + z

    # the input and output oriented throughflow normalized environ analysis
    
    # Direct throughflow 
    FD = F - diag(T)				# flow matrix with negative throughflows on diagonal
    G = I + FD %*% solve(diag(T)) 	   # fij/Tj for i,j=1:n -- output matrix
    GP = I + solve(diag(T)) %*% FD         # fij/Ti for i,j=1:n -- input matrix
    
    # Integral throughflow 
    N = solve(I-G)		    # integral output flow matrix -- I+G+G^2+G^3+...
    NP = solve(I-GP)		  # integral input flow matrix -- I+GP+GP^2+GP^3+...
    dN = diag(N)

    modes = flow.modes(N,z)

    # Throughflow environ properties
    p = rep(1,n)		    	       # ones vector
    TSTc = sum(((dN-p)/dN)*T)  # cycled (mode 2) throughflow
    TST = sum(T)		             # total system throughflow
    CIF = TSTc/TST	    		     # cycling index (modified from Finn 1976)
    Z = sum(z)                   # boundary flow

    # Amplification parameter
    NAF  = length( which( ( N-diag(dN)) > 1) )       # output 
    NAFP = length( which( (NP-diag(diag(NP)))>1))       # input 

    # Indirect effects parameter
    IDF  = sum(N-I-G)/sum(G)    	   # indirect to direct ratio (output)
    IDFP = sum(NP-I-GP)/sum(GP)  	   # indirect to direct ratio (input)	

    # Homogenization parameter
    CVG = sd(as.vector(G))/mean(G) 	    	 # coefficient of variation for G
    CVN = sd(as.vector(N))/mean(N) 	    	 # coefficient of variation for N
    HF = CVG/CVN				        	#  homogenization parameter (output)

    CVGP = sd(as.vector(GP))/mean(GP)			# coefficient of variation for G
    CVNP = sd(as.vector(NP))/mean(NP)			# coefficient of variation for N
    HFP = CVG/CVN 				        	    # homogenization parameter (input)

    # Network Aggradation or Average Path Length
    AGG = TST / Z   # Jorgensen, Patten and Straskraba (2000)
                    # Original formulation of average path length (Finn 1976)
                    # This parameter is expected to increase as systems develop.

    flow_ep = list (TST=TST, CIF=CIF, modes=modes, NAF=NAF, NAFP=NAFP, IDF=IDF, IDFP=IDFP, HF=HF, HFP=HFP, AGG=AGG, G=G, GP=GP, N=N, NP=NP)
    return(flow_ep)
  }


