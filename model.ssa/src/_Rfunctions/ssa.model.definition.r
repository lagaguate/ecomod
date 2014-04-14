


  ssa.model.definition = function( p=NULL, DS = "logistic", increment=1L ) {

    if (is.null(p)) p = list()
        
    
    if ( DS %in% c("logistic", "logistic.no.movement") ) {

      p <- within( p, {

        RE = function( p, X, ix=1:length(X) ) {
          with(p, { 
            # incremental updates of state (XX) & propensity
            # intermediary calculations that get reused .. marginally speeds up a few of the following calculations
            bx = b[ix]*X
            dx = d[ix]*X
            x_k = X/K[ix]
            c( 
              bx + dx*x_k,    # birth process -- unary process .. second elements are dummy space-filling values
              dx + bx*x_k    # death process
            )
          }) # end with
        }  # end RE

   
        # Changes associated with Reaction processes 
        # Lagrangian operator structure: 
        #   (row, column, operation) 
        # where row, column are relative to the focal cell 
        NU = c (
          c(0,0, increment), c(0,  0, 0)  , # for the focal cell (0,0), the birth process: "bX" .. second set of zeros are dummy place holders
          c(0,0,-increment), c(0,  0, 0)   # for the focal cell (0,0), the death process: "(d+(b-d)*X/K)*X""
        )
     
        np = 2  # no. of processes
        NU = array ( NU, dim = c(3, 2, np) ) # dim1 are row,col,operation; dim2 are focal cell(s), unary operations have second operation as 0; dim3 is number of reaction channels
        nP = nc*nr*np    # total number of cell in the propensity matrix
      }) # first within
    }

      
    if ( DS %in% c( "logistic.randomwalk", "logistic,diffusion") ) {

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
              bx + dx*x_k,  # birth process 
              dx + bx*x_k,  # death process
              Da * c(X,X,X,X) # advection --- >>> the diffusion comes from its' stochasticity and with a magnitude of (u+d)^/2
            )
          }) # end with
        }  # end RE


        # Changes associated with Reaction processes 
        # Lagrangian operat x or structure: 
        #   (row i, column j, operation) 
        # where row, column are relative to the focal cell 
        NU = c( 
          c(0,0, increment), c(0,  0, 0) , # for the focal cell (0,0), the birth process: "bX" .. second set of zeros are dummy place holders
          c(0,0,-increment), c(0,  0, 0) , # for the focal cell (0,0), the death process: "(d+(b-d)*X/K)*X""
          # row-wise movements (i)
          c( 0,  0, -increment), c(-1,  0, +increment) , # move "left":: X[i,]   -> X[i-1,]  .. all left-ward components  
          c( 0,  0, -increment), c(+1,  0, +increment) , # move:: X[i,] -> X[i+1,]
          # column-wise movements (j)
          c( 0,  0, -increment), c( 0, -1, +increment) , # move:: X[,j] -> X[,j-1]
          c( 0,  0, -increment), c( 0, +1, +increment)   # move:: X[,j] -> X[,j+1]
          )

        np = 6  # no. of processes
        NU = array ( NU, dim = c(3, 2, np) ) # dim1 are row,col,operation; dim2 are focal cell(s), unary operations have second operation as 0; dim3 is number of reaction channels
        nP = nc*nr*np    # total number of cell in the propensity matrix
      })
    }


    if ( DS %in% c("logistic.correlated.randomwalk", "logistic.advection.diffusion") ) {

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
              bx + dx*x_k,    # birth process -- unary process ing values
              dx + bx*x_k,    # death process
              move_velocity * c(X,X,X,X) * c( Hr0[ix], Hr1[ix], Hc0[ix], Hc1[ix] )   # advection --- >>> the diffusion comes from its' stochasticity and with a magnitude of (u+d)^/2
            )
          }) # end with
        }  # end RE

        # Changes associated with Reaction processes 
        # Lagrangian operator structure: 
        #   (row, column, operation) 
        # where row, column are relative to the focal cell 
        NU = c (
          c(0,0, increment), c(0,  0, 0)  , # for the focal cell (0,0), the birth process: "bX"
          c(0,0,-increment), c(0,  0, 0)  , # for the focal cell (0,0), the death process: "(d+(b-d)*X/K)*X""
          c(0,0,-increment), c(-1,0,increment) , # advection jumps :: X[i] -> X[i-1] -- negative index direction
          c(0,0,-increment), c(+1,0,increment) ,
          c(0,0,-increment), c(0,-1,increment) ,  # same as above but now for column-wise jumps advection
          c(0,0,-increment), c(0,+1,increment) 
        )
 
        np = 6  # no. of processes
        NU = array ( NU, dim = c(3, 2, np) ) # dim1 are row,col,operation; dim2 are focal cell(s), unary operations have second operation as 0; dim3 is number of reaction channels

        nP = nc*nr*np    # total number of cell in the propensity matrix
      })
    }
 

    # -------------------
    

    if ( DS %in% c( "stage.based.snow.crab", "stage.based.advection.diffusion.snow.crab") ) {  # similar to a delay-difference model with no recruitment

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
              dx + bx*x_k,   # death process
              move_velocity * c(X,X,X,X) * c( Hr0[ix], Hr1[ix],Hc0[ix], Hc1[ix] ),   # advection --- >>> the diffusion comes from its' stochasticity and with a magnitude of (u+d)^/2
 
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
        NU = c (
          c(0,0, increment), c(0,  0, 0)  , # for the focal cell (0,0), the birth process: "bX"
          c(0,0,-increment), c(0,  0, 0)  , # for the focal cell (0,0), the death process: "(d+(b-d)*X/K)*X""
              #  ... unfinished 
  
        )

        np = xxx  # no. of processes
        NU = array ( NU, dim = c(3, 2, np) ) # dim1 are row,col,operation; dim2 are focal cell(s), unary operations have second operation as 0; dim3 is number of reaction channels
        nP = nc*nr*np    # total number of cell in the propensity matrix
    
      }) # first within
    }


    return(p)
  }


