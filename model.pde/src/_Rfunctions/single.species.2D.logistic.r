

single.species.2D.logistic <- function (modeltime, state, pars ) {

  with ( pars, {
    
      # total reaction-diffusion-advection equation: -- note the negative sign for advection
      # d/dt{A} = - d/dx{w*A} + d/dx{ Da*d/dx{A} } + Reaction
      
      # Solution via Method of Lines Finite Element discretization 

  
    # STATE variables
      A = matrix(ncol=nc, nrow=nr, data=state ) # reshape vector to matrix form
      dA = A*0


    # some indices --- to permit indexing for 1st and second order diffferencing
      i = 2:(nr-1)
      j = 2:(nc-1)


    # REACTION

      if ( any( grep( "reaction", p$parmeterizations, ignore.case=TRUE)) ) {

        dA = dA + r * A * (1 - A/K)

        rm(r, K); gc()
      }

    # ADVECTION -- directed flow due to habitat preferences
    # - d/dx{ w*A } ; w is velocity component
    # using 1st order upwind finite discretization -- higher orders add oscillations
    # expectation that Pr(l-r) ~ mean velocity         
 
      # H = model.pde.external.db( p=pars, method="depth", variable="t" )
      # Va = model.pde.external.db( p=pars, method="habitat", variable="habitat.mean"  )
 
      if ( any( grep( "advection", p$parmeterizations, ignore.case=TRUE)) ) {
          
        if ( is.list( Va ) ) {
          Vr = Va[[1]]
          Vc = Va[[2]]
        } else {
          Vr = Vc = Va
        }

        hr = hc = A * 0

        # some indices
        i = 2:(nr-1)
        j = 2:(nc-1)
                
        hc[,j] = Vc[,j] * (A[,j] - A[,j-1])/dc  # flux across cols - right is positive
        hr[i,] = Vr[i,] * (A[i,] - A[i+1,])/dr  # flux up rows - up positive (but in R up is negative ... 1+1 is upwind )

        dA = dA - ( hc + hr )  # note the sign 
        
        rm( Vr, Vc, hc, hr); gc()
       
      }


    # DIFFUSION  
    # d/dx{ Da * d/dx{A} }  ## 'd' == partial derivative
 
      if ( any( grep( "diffusion", p$parmeterizations, ignore.case=TRUE)) ) {
       
        if ( is.list( Da ) ) {
          Dr = Da[[1]]
          Dc = Da[[2]]
        } else {
          Dr = Dc = Da 
        }

        
        if ( "diffusion.first.order.upwind" %in% p$parmeterizations ) {
          # flux up rows - up positive (but in R up is negative ... invert)
 
          Gc = Gr = Gcc = Grr = A * 0
          Gc[,j] = Dc[,j] * ( A[,j] - A[,j-1] ) / dc
          Gr[i,] = Dr[i,] * ( A[i,] - A[i+1,] ) / dr
          Gcc[,j] = ( Gc[,j] - Gc[,j-1] ) / dc
          Grr[i,] = ( Gr[i,] - Gr[i+1,] ) / dr
          rm( Dc, Dr, Gc, Gr); gc()
        
        } else if ("diffusion.second.order.central" %in% p$parmeterizations ) {
        
          Gcc = Grr = A * 0
          Gcc[,j] = Dc[,j] * (A[,j+1] - 2*A[,j] + A[,j-1] ) / dc^2
          Grr[i,] = Dr[i,] * (A[i-1,] - 2*A[i,] + A[i+1,] ) / dr^2
          rm( Dc, Dr); gc()
        
        } else {
          stop ("Must define diffusion discretization") 
        }

        dA = dA + ( Grr + Gcc )
      
      }

    return( list( as.vector( dA  ) ))
  
  })
}




