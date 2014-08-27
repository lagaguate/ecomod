 
  variable.recode = function( x, variable, direction="forward", db="snowcrab", rm.na=F ) {
    
    TF = transf.lookup( variable, db )
    
    if ( TF$transform %in% c("", "none") ) {
      B = x
    }
 
    if ( TF$transform == "log10") {
      if (direction =="forward") {
        B = log10( x + TF$offset )
      } else if (direction =="backward") {
        B = 10^x - TF$offset
      }
    }

    if ( TF$transform == "scaled+centered") {
      if (direction =="forward") {
        B = scale( x, center=TF$offset, scale=TF$scaling )
      } else if (direction=="backward" ) {
        B = fields::unscale( x, x.center=TF$offset, x.scale=TF$scaling )
      }
    }

    if (rm.na) B = B[which(is.finite(B))]
    
    return (B)
  }


