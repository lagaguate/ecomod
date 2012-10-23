
  factor2number = function(x, vars) {
    for (i in vars) {
			if ( is.factor( x[,i]	) ) {
				x[,i] = as.numeric(levels( x[,i] ))[as.integer( x[,i] )]
			} else if ( is.character( x[,i] ) ) {
				x[,i] = as.numeric(as.character(x[,i]))
			}
		}
		return(x)
  }


