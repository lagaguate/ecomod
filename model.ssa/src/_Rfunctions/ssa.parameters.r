



  ssa.parameters= function( p=NULL, ptype = "debug" ) {

    if (is.null(p)) p = list()
      
    p <- within( p, { 
      eps  = 1e-6   # A in units of t/km^2 -- number below which abundance can be considered zero ~ 1 kg/ km^2 = 1g / m^2
      atol = 1e-9  # atol -- absolute error tolerance for lsoda
      rtol = 1e-9  # rtol -- relative error tolerance for lsoda
    })

    if ( ptype=="systemsize.debug" ) {
      
      p <- within( p, {
        ############## Spatial extents
        # size of the spatial components
        # rows are easting (x);  columns are northing (y) --- in R 
        # ... each cell has dimensions of 1 X 1 km ^2
        nr = 100   # rows
        nc = 100   # cols
        nrc = nr*nc  # total number of cells
        
        # calc these here to avoid repeating calculations in the innermost loop
        nr_1 = nr-1
        nc_1 = nc-1
       
       })
    }
 
    
    if ( ptype=="logistic.debug" ) {
      
      p <- within( p, {
        
        ############## Model Parameters 
        # Basic logistic with spatial processes  
        # Using: logistic model as base
        # dX/dt = rX(1-X/K)
        # in the stochastic form:: using a birth-death Master Equation approach 

        # reaction (population dynamic) componenet of the model 
        b = matrix( nrow=nr, ncol=nc, data=3/365 ) # birth rate
        d = matrix( nrow=nr, ncol=nc, data=2/365 ) # death rate
        r = b-d # not used in SSA but must match above for PDE
        K = matrix( ncol=nc, nrow=nr, data=100 )
        # K = matrix( nrow=p$nr, ncol=p$nc, data=rnorm( p$nrc, mean=p$K, sd=p$K/10) )
        # K[ inothabitat ] = eps
        Da = diffusioncoeff= 0.5  # km^2/day  .. see /home/jae/ecomod/model.pde/src/_Rfunctions/estimate.bulk.diffusion.coefficient.r
        # directed movement .. using a Random uniform distrubution for testing
        move_velocity =  exp(-2.729117)
        rrr = rep(1, nc)  # row vector of 1's
        ccc = rep(1, nr)  
        H = matrix( nrow=nr, ncol=nc, data=eps )
        c0 = round( nc/2 )
        bw = round( nc / 10 / 2 )
        H [, (c0-bw) : (c0+bw) ] = 1
        H = H * runif( H, min=0.8, max=1)  # make a central band "optimal"   
        Hr = H[1:(nr-1),] /  H[2:nr,] 
      # a ratio of two probabilities should have ~ normal distribution (?? source ??)
      # using a quantile-based truncation at p=0.025, and p=0.975 
      Qr = quantile( Hr, probs=c(0.025, 0.975), na.rm=T )
      Hr[ Hr < Qr[1] ] = Qr[1] 
      Hr[ Hr > Qr[2] ] = Qr[2] 
        
        
        Hr0 = rbind( rrr, Hr )  # hazzard ratio of up moving across rows in the negative direction
        Hr1 = rbind( 1/Hr, rrr ) # down positive
        Hc = H[,1:(p$nc-1)] /  H[,2:p$nc]
      Qc = quantile( Hc, probs=c(0.025, 0.975), na.rm=T )
      Hc[ Hc < Qc[1] ] = Qc[1] 
      Hc[ Hc > Qc[2] ] = Qc[2] 
           
        
        Hc0 = cbind( ccc, Hc) # hazzard ratio of Pr of moving in negative direction
        Hc1 = cbind( 1/Hc, ccc ) #  positive direction
        H = Hc = Hr = rrr = ccc = NULL
       })
    }

   
    if ( ptype=="simtimes.debug" ) {
   
      p <- within( p, { 
        n.times = 10 # 365  # number of censuses  
        t.end =   10 # 365   # in model time .. days
        t.censusinterval = t.end / n.times
        modeltimeoutput = seq( 0, t.end, length=n.times )  # times at which output is desired .. used by pde
      })
    }

    return(p)
  }


