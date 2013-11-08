



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
        b = 2 / 365 # birth rate
        d = 1 / 365 # death rate
        K = 100   # "carrying capacity"
        r = b - d  ## used by pde model  # r = b-d >0 

        # diffusion parameters 
        # coef d=D/h^2 ; h = 1 km; per year (range from 1.8 to 43  ) ... using 10 here 
        # ... see bulk estimation in model.lattice/src/_Rfunctions/estimate.bulk.diffusion.coefficient.r
        DaR = 10 
        DaC = 10 
        Da = matrix( ncol=nc, nrow=nr, data=10 )
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


