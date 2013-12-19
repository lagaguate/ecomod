


  ssa.model.definition = function( p=NULL, ptype = "logistic", increment=1 ) {

    if (is.null(p)) p = list()
        
    
    if ( ptype=="logistic" ) {

      p <- within( p, {

        RE0 = function(p, X) { 
          with( p, { 
            c( 
              b*X + d*X*X/K,  # birth process
              d*X + b*X*X/K   # death process
            )
          })
        }


        RE = function( p, res, jn, jj ) {
          with( p, { 
            # determine focal cell coords
            cc = floor( (jj-1)/nr ) + 1   # col
            cr = jj - (cc-1) * nr         # row

            # determine the appropriate operations for the reaction and their 'real' locations
            o = NU[[ jn ]]  
            no = dim(o)[1]  # nrows = # operations (ie, unary, or binary)
            
            ro = cr + o[,1]  # row of the focal cell
            co = cc + o[,2]  # column of the focal cell
            
            # ensure boundary conditions are sane
            ro[ro < 1] = 2
            ro[ro > nr] = nr_1
            
            co[co < 1] = 2
            co[co > nc] = nc_1 
            
            # if (1 %in% co)  browser()

            # build correctly structured indices 
            ix = cbind( ro, co)  ## row, column in X
            ip = cbind( ro, co, po[[no]] )   # rows and columns in P 

            # update state (X) 
            XX = res$X[ix] + o[,3]  
            XX[ XX < 0 ] = 0
            res$X[ix] = XX

            # propensity  (reaction rate) calculating function .. returns as a vector of reaction process rates ...
            bb = b[ix]
            dd = d[ix]
            KK = K[ix]

            PP = c( 
              bb*XX + dd*XX*XX/KK,  # birth process
              dd*XX + bb*XX*XX/KK   # death process
            )
            
            res$P.total = res$P.total + sum( res$P[ip] - PP )
            res$P[ip] = PP

            return( res )

          })
        }
    
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

      
    if ( ptype=="logistic.randomwalk" ) {

      p <- within( p, {
         
        # propensity calculating function .. returns as a vector of reaction process rates ...
        # this diffusion parameterization is correct when: Da = DiffCoeff / h^2 ; h=1 in this case
    
        RE0 = function(p, X) { 
          with(p, {
            Dax = Da*X
            c(  
              b*X + d*X*X/K,  # birth process
              d*X + b*X*X/K,  # death process
              Dax, Dax, Dax, Dax  # diffusion components
            ) ## return only P
          })
        }

         
        RE = function( p, res, jn, jj ) {
          with(p, { 
            # determine focal cell coords
            cc = floor( (jj-1)/nr ) + 1   # col
            cr = jj - (cc-1) * nr         # row
            # determine the appropriate operations for the reaction and their 'real' locations
            o = NU[[ jn ]]  
            no = dim(o)[1]  # nrows = # operations (ie, unary, or binary)
            ro = cr + o[,1]  # row of the focal cell
            co = cc + o[,2]  # column of the focal cell
            # ensure boundary conditions are sane
            ro[ro < 1] = 2
            ro[ro > nr] = nr_1
            co[co < 1] = 2
            co[co > nc] = nc_1 
            # build correctly structured indices 
            ix = cbind( ro, co)  ## row, column in X
            ip = cbind( ro, co, po[[no]] )   # rows and columns in P 
            # update state (XX) 
            XX = res$X[ix] + o[,3]  
            XX[ XX < 0 ] = 0
            res$X[ix] = XX
            bb = b[ix]
            dd = d[ix]
            KK = K[ix]
            # indices of areas surrounding the focal point
            Dax = Da*XX
            PP = c( 
              bb*XX + dd*XX*XX/KK,  # birth process
              dd*XX + bb*XX*XX/KK,   # death process
              Dax, Dax, Dax, Dax       # diffusion components
            )
            
            res$P.total = res$P.total + sum( res$P[ip] - PP )
            res$P[ip] = PP

            return( res )
          })
        }
        
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


    if ( ptype=="logistic.correlated.randomwalk" ) {

      p <- within( p, {
           
        RE0 = function(p, X) { 
          with(p, { 
            Dax = Da*X
            adv = move_velocity*X
            c(  
              b*X + d*X*X/K,  # birth process
              d*X + b*X*X/K,   # death process
              adv*Hr0, adv*Hr1, adv*Hc0, adv*Hc1,  # directed motion
              Dax, Dax, Dax, Dax  # diffusion components
            )
          })
        }  # end RE
               

        # propensity calculating function .. returns as a vector of reaction process rates ...
        RE = function( p, res, jn, jj ) {
          with(p, { 
            # determine focal cell coords
            cc = floor( (jj-1)/nr ) + 1   # col
            cr = jj - (cc-1) * nr         # row
            # determine the appropriate operations for the reaction and their 'real' locations
            o = NU[[ jn ]]  
            no = dim(o)[1]  # nrows = # operations (ie, unary, or binary)
            ro = cr + o[,1]  # row of the focal cell
            co = cc + o[,2]  # column of the focal cell
            # ensure boundary conditions are sane
            ro[ro < 1] = 2
            ro[ro > nr] = nr_1
            co[co < 1] = 2
            co[co > nc] = nc_1 
            # build correctly structured indices 
            ix = cbind( ro, co)  ## row, column in X
            ip = cbind( ro, co, po[[no]] )   # rows and columns in P 
            # update state (XX) 
            XX = res$X[ix] + o[,3]  
            XX[ XX < 0 ] = 0
            res$X[ix] = XX
            bb = b[ix]
            dd = d[ix]
            KK = K[ix]
            # indices of areas surrounding the focal point
            Dax = Da*XX
            adv = move_velocity*XX
            PP = c(
              bb*XX + dd*XX*XX/KK,  # birth process
              dd*XX + bb*XX*XX/KK,   # death process
              adv*Hr0[ix], adv*Hr1[ix], adv*Hc0[ix], adv*Hc1[ix] ,  # advection
              Dax, Dax, Dax, Dax  # diffusion
            )

            # browser()

            res$P.total = res$P.total + sum( res$P[ip] - PP )
            res$P[ip] = PP

            return( res )
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
          rbind( c(0,0,-increment), c(0,+1,increment) ),
          rbind( c(0,0,-increment), c(-1,0,increment) ), # diffusion jumps :: X[i] -> X[i-1] -- negative index direction
          rbind( c(0,0,-increment), c(+1,0,increment) ),
          rbind( c(0,0,-increment), c(0,-1,increment) ),  # same as above but now for column-wise jumps
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
    

    if ( ptype=="stage.based.snow.crab" ) {  # similar to a delay-difference model with no recruitment

      p <- within( p, {

        RE0 = function(p, X, X1) { 
          with( p, { 
            c( 
              pr.moult*pr.growth*Xss, # birth process (i.e. moult Prob = 1 / 365; and pr of growth ) * (Xss = soft-shelled) 
              m*X,            # constant death process
            )
          })
        }


        RE = function( p, res, jn, jj ) {
          with( p, { 
            # determine focal cell coords
            cc = floor( (jj-1)/nr ) + 1   # col
            cr = jj - (cc-1) * nr         # row

            # determine the appropriate operations for the reaction and their 'real' locations
            o = NU[[ jn ]]  
            no = dim(o)[1]  # nrows = # operations (ie, unary, or binary)
            
            ro = cr + o[,1]  # row of the focal cell
            co = cc + o[,2]  # column of the focal cell
            
            # ensure boundary conditions are sane
            ro[ro < 1] = 2
            ro[ro > nr] = nr_1
            
            co[co < 1] = 2
            co[co > nc] = nc_1 
            
            # if (1 %in% co)  browser()

            # build correctly structured indices 
            ix = cbind( ro, co)  ## row, column in X
            ip = cbind( ro, co, po[[no]] )   # rows and columns in P 

            # update state (X) 
            XX = res$X[ix] + o[,3]  
            XX[ XX < 0 ] = 0
            res$X[ix] = XX

            # propensity  (reaction rate) calculating function .. returns as a vector of reaction process rates ...
            bb = b[ix]
            dd = d[ix]
            KK = K[ix]

            PP = c( 
              bb*XX + dd*XX*XX/KK,  # birth process
              dd*XX + bb*XX*XX/KK   # death process
            )
            
            res$P.total = res$P.total + sum( res$P[ip] - PP )
            res$P[ip] = PP

            return( res )

          })
        }
    
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


    return(p)
  }


