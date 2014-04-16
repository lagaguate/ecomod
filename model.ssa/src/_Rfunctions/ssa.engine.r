
  # simple lookup of the ssa.engine and run it

  ssa.engine = function(...) {  # another function level is required to break down the ellipsis ...
    
    starttime = Sys.time()

    params <- list(...)
    p <- params[[1]]
    SE = switch( p$ssa.method, 
      exact = ssa.engine.exact,   # using the exact solution ... ~ 1 day -> every 25-30 minutes
      approximation = ssa.engine.approximation ,
      fast = ssa.engine.approximation.rcpp,
      default = ssa.engine.approximation.rcpp 
     )
    res = SE( ... ) 

    endtime = Sys.time()
    print( endtime-starttime )

    return(res)
  }


