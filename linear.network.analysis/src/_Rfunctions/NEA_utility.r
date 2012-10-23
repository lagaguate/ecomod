  
  NEA_utility = function( F, x ) {
  
    n = nrow(F)
    I = diag(n)
    T = colSums( t(F) ) + z

 
    # Direct Utility, Throughflow 
    FD = F - diag(T)				# flow matrix with negative throughflows on diagonal
    D = solve( diag(T)) %*% ( FD - t(FD) )	# (fij-fji)/Ti for i,j=1:n, (GP-G') -- utility matrix
    e = eigen(D)		    		# convergence test
    if (abs(max(Mod(e$values)))>=1) {           # check for convergence
      print( "WARNING: Throughflow Utility matrix does not converge")
      U = Y = NSF = PNF = -9999;                #  flag if no convergence
    } else { 
      # Integral Utility, Throughflow
      U = solve(I-D)          # Nondimensional integral flow utility
      Y = diag(T) %*% U		    # Dimensional integral flow utility
        
      # Throughflow Utility Indices    
      NSF = bcratio(Y)		    # flow benefit cost ratio (calls other function) (Synergism)
      B = matrix( c(1, 1, 1, -1 ), nrow=2, byrow=T )			# coefficient matrix
      Z = c(n^2, sum(sign(U)))   # vector with total n and addition of all entries
#     X = B \ Z 					# solve for number of positive and negative signs    # solve BX=Z
      X = solve(B) %*% Z

      PNF = X[1,1] / X[2,1]		# ratio of positive to negative signs (mutualism)
    }

    # Direct Utility, Storage 
    DS = solve( diag(x)) %*% (FD-t(FD))   # (fij-fji)/xi for i,j=1:n, (CP-C') -- utility matrix
    e = eigen(DS)
    if (abs(max(Mod(e$values)))>=1 ) {      # check for convergence
      print("WARNING: Storage Utility matrix does not converge")

      #  Integral Utility, Storage
      US = YS = NSS = PNS = -9999	# flag if no convergence
    } else {

      #  Integral Utility, Storage
      US = solve( I-DS )    # Nondimensional integral storage utility
      YS = diag(T) %*% US		# Dimensional integral storage utility
        
      # Storage Utility Indices
      NSS = bcratio(YS) 	# storage benefit cost ratio (calls other function)
      B =  matrix( c(1, 1, 1, -1 ), nrow=2, byrow=T )   # coefficient matrix
      Z = c(n^2, sum(sign(US)))    # vector with total n and addition of all entries
  #   X = B \ Z				# solve for number of positive and negative signs  # solve BX=Z
      X = solve(B) %*% Z

      PNS = X[1,1] / X[2,1]	  # storage ratio of positive to negative signs
    }     

    utility_ep = list(NSF=NSF, PNF=PNF, NSS=NSS, PNS=PNS)
    return ( utility_ep )
  }


