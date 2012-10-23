
  bcratio = function(Y) {

#  This calculates the ratio of sum of positive to sum of
#  negative interactions in the system.  
#  The B matrix sets up a pair of linear equations where
#  pos+neg=sum(sum(abs(Y))) and pos-neg=sum(sum(Y))
#  This set of equations is solved, X, and a ratio is taken.
#  The next line zeros out the diagonal elements
#  Y=Y-diag(diag(Y));

    plus = sum(abs(Y))
    minus = sum(Y)
    B = matrix( c(1, 1, 1, -1 ), nrow=2, byrow=T ) 
    Z = rbind(plus, minus)
#    X = B\Z  # solve BX=Z
    X = solve(B) %*% Z
    r = X[1,1] / X[2,1]

    return(r)
  }


