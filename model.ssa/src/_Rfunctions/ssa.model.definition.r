


  ssa.model.definition = function( p=NULL, ptype = "logistic", increment=1L ) {

    if (is.null(p)) p = list()
        
    
    if ( ptype %in% c("logistic", "logistic.no.movement") ) {

      p <- within( p, {

        RE = function( p, X, ix=1:length(X) ) {
          with(p, { 
            # incremental updates of state (XX) & propensity
            # intermediary calculations that get reused .. marginally speeds up a few of the following calculations
            bx = b[ix]*X
            dx = d[ix]*X
            x_k = X/K[ix]
            c( 
              bx + dx*x_k,   # birth process
              dx + bx*x_k   # death process
            )
          }) # end with
        }  # end RE

   
        # Changes associated with Reaction processes 
        # Lagrangian operator structure: 
        #   (row, column, operation) 
        # where row, column are relative to the focal cell 
        NU = list (
          rbind( c(0,0, increment) ),  # for the focal cell (0,0), the birth process: "bX"
          rbind( c(0,0,-increment) )  # for the focal cell (0,0), the death process: "(d+(b-d)*X/K)*X""
        )

        np = length(NU)  # no. of processes
        nP = nc*nr*np    # total number of cell in the propensity matrix
 
        # pre-sorted data indices ('pox') that permit rapid transcription of propensity calculations, 
        # that depends upon if they are unary or binary interactions
        po = list(
          rep(1:np), # for 1 set
          c(t(matrix( rep(1:np,2), ncol=2)))  # for 2 sets 
        )
    
      }) # first within
    }

      
    if ( ptype %in% c( "logistic.randomwalk", "logistic,diffusion") ) {

      p <- within( p, {
        # propensity calculating function .. returns as a vector of reaction process rates ...
        # this diffusion parameterization is correct when: Da = DiffCoeff / h^2 ; h=1 in this case

        RE = function( p, X, ix=1:length(X) ) {
          with(p, { 
            # incremental updates of state (XX) & propensity
            # intermediary calculations that get reused .. marginally speeds up a few of the following calculations
            bx = b[ix]*X
            dx = d[ix]*X
            x_k = X/K[ix]
            c( 
              bx + dx*x_k,   # birth process
              dx + bx*x_k,   # death process
              Da * c(X, X, X, X) # fixed diffusion coef
            )
          }) # end with
        }  # end RE


        # Changes associated with Reaction processes 
        # Lagrangian operat x or structure: 
        #   (row i, column j, operation) 
        # where row, column are relative to the focal cell 
        NU = list (
          rbind( c(0,0, increment) ),  # for the focal cell (0,0), the birth process: "bX"
          rbind( c(0,0,-increment) ), # for the focal cell (0,0), the death process: "(d+(b-d)*X/K)*X""
          # row-wise movements (i)
          rbind( c( 0,  0, -increment), c(-1,  0, +increment) ), # move "left":: X[i,]   -> X[i-1,]  .. all left-ward components  
          rbind( c( 0,  0, -increment), c(+1,  0, +increment) ), # move:: X[i,] -> X[i+1,]
          # column-wise movements (j)
          rbind( c( 0,  0, -increment), c( 0, -1, +increment) ), # move:: X[,j] -> X[,j-1]
          rbind( c( 0,  0, -increment), c( 0, +1, +increment) )  # move:: X[,j] -> X[,j+1]
        )

        np = length(NU)  # no. of processes
        nP = nc*nr*np    # total number of cell in the propensity matrix
        
        # pre-sorted data indices ('pox') that permit rapid transcription of propensity calculations, 
        # that depends upon if they are unary or binary interactions
        po = list(
          rep(1:np), # for 1 set
          c(t(matrix( rep(1:np,2), ncol=2)))  # for 2 sets 
        )

      })
    }


    if ( ptype %in% c("logistic.correlated.randomwalk", "logistic.advection.diffusion") ) {

      p <- within( p, {
        # propensity calculating function .. returns as a vector of reaction process rates ...
        # this diffusion parameterization is correct when: Da = DiffCoeff / h^2 ; h=1 in this case
 
        RE = function( p, X, ix=1:length(X) ) {
          with(p, { 
            # incremental updates of state (XX) & propensity
            # intermediary calculations that get reused .. marginally speeds up a few of the following calculations
            bx = b[ix]*X
            dx = d[ix]*X
            x_k = X/K[ix]
            c( 
              bx + dx*x_k,   # birth process
              dx + bx*x_k,   # death process
              move_velocity * c(X, X, X, X) * c( Hr0[ix], Hr1[ix], Hc0[ix], Hc1[ix] )   # advection --- >>> the diffusion comes from its' stochasticity and with a magnitude of (u+d)^/2
            )
          }) # end with
        }  # end RE

        # Changes associated with Reaction processes 
        # Lagrangian operator structure: 
        #   (row, column, operation) 
        # where row, column are relative to the focal cell 
        NU = list (
          rbind( c(0,0, increment) ),  # for the focal cell (0,0), the birth process: "bX"
          rbind( c(0,0,-increment) ), # for the focal cell (0,0), the death process: "(d+(b-d)*X/K)*X""
          rbind( c(0,0,-increment), c(-1,0,increment) ), # advection jumps :: X[i] -> X[i-1] -- negative index direction
          rbind( c(0,0,-increment), c(+1,0,increment) ),
          rbind( c(0,0,-increment), c(0,-1,increment) ),  # same as above but now for column-wise jumps advection
          rbind( c(0,0,-increment), c(0,+1,increment) )
        )

        np = length(NU)  # no. of processes
        nP = nc*nr*np    # total number of cell in the propensity matrix

        # pre-sorted data indices ('pox') that permit rapid transcription of propensity calculations, 
        # that depends upon if they are unary or binary interactions
        po = list(
          rep(1:np), # for 1 set
          c(t(matrix( rep(1:np,2), ncol=2)))  # for 2 sets 
        )

      })
    }
 

    # -------------------
    

    if ( ptype %in% c( "stage.based.snow.crab", "stage.based.advection.diffusion.snow.crab") ) {  # similar to a delay-difference model with no recruitment

      p <- within( p, {

        RE = function(p, X, X1) { 
          with( p, { 
            array( c( 
              pr.moult*pr.growth*Xss, # birth process (i.e. moult Prob = 1 / 365; and pr of growth ) * (Xss = soft-shelled) 
              m*X,            # constant death process
            ), dim=c( nr, nc, np ) ) 
          })
        }
       
        RE = function( p, X, ix=1:length(X) ) {
          with(p, { 
            # incremental updates of state (XX) & propensity
            # intermediary calculations that get reused .. marginally speeds up a few of the following calculations
            bx = b[ix]*X
            dx = d[ix]*X
            x_k = X/K[ix]
            c( 
              pr.moult*pr.growth*Xss, # birth process (i.e. moult Prob = 1 / 365; and pr of growth ) * (Xss = soft-shelled) 
              m*X,            # constant death process
              b*Recruits,   # birth process
              move_velocity * c(X, X, X, X) * c( Hr0[ix], Hr1[ix], Hc0[ix], Hc1[ix] ) ,  # advection
              move_velocity * c(R, R, R, R) * c( Hr0[ix], Hr1[ix], Hc0[ix], Hc1[ix] )   # advection
            )
          }) # end with
        }  # end RE

   
        # Changes associated with Reaction processes 
        # Lagrangian operator structure: 
        #   (row, column, operation) 
        # where row, column are relative to the focal cell 
        NU = list (
          rbind( c(0,0, increment) ),  # for the focal cell (0,0), the birth process: "bX"
          rbind( c(0,0,-increment) )  # for the focal cell (0,0), the death process: "(d+(b-d)*X/K)*X""
          #  ... unfinished 
        )

        np = length(NU)  # no. of processes
        nP = nc*nr*np    # total number of cell in the propensity matrix
 
        # pre-sorted data indices ('pox') that permit rapid transcription of propensity calculations, 
        # that depends upon if they are unary or binary interactions
        po = list(
          rep(1:np), # for 1 set
          c(t(matrix( rep(1:np,2), ncol=2)))  # for 2 sets 
        )
    
      }) # first within
    }


    return(p)
  }


