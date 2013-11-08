


  ssa.model.definition = function( p=NULL, ptype = "default.logistic" ) {

    if (is.null(p)) p = list()
    
    if ( ptype=="default.logistic" ) {

      p <- within( p, {
         
        # propensity calculating function .. returns as a vector of reaction process rates ...
        RE = function( X, b, d, K, DaR, DaC) {
          c(
            b[]*X[] ,
            (d[]+(b[]-d[])*X[]/K)[]*X[] ,
            DaR[]*X[] ,
            DaR[]*X[] ,
            DaC[]*X[] ,
            DaC[]*X[] 
        )}
 
        # Changes associated with Reaction processes 
        # Lagrangian operator structure: 
        #   (row, column, operation) 
        # where row, column are relative to the focal cell 
        NU = list (
          rbind( c(0,0,1) ),  # for the focal cell (0,0), the birth process: "bX"
          rbind( c(0,0,-1) ), # for the focal cell (0,0), the death process: "(d+(b-d)*X/K)*X""
          rbind( c(0,0,-1), c(-1,0,1) ), # "jump to adjacent row from the focal cell:: X[i] -> X[i+/-1] {dr0}
          rbind( c(0,0,-1), c(+1,0,1) ),
          rbind( c(0,0,-1), c(0,-1,1) ),  # same as above but now for column-wise jumps
          rbind( c(0,0,-1), c(0,+1,1) )
        )

        np = length(NU)  # no. of processes
        
        # pre-sorted data indices ('pox') that permit rapid transcription of propensity calculations, 
        # that depends upon if they are unary or binary interactions
        po = list(
          rep(1:6), # for 1 set
          c(t(matrix( rep(1:6,2), ncol=2)))  # for 2 sets 
        )

      })
    }

    
    return(p)
  }


