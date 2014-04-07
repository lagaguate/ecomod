
ssa.engine.exact = function( p, res ) {

  res <- with (p, { 
    on.exit( return(res ))    # to debug
    tio = tout = 0
    
    # indices used for id reaction channel (v) -- 2x as they are binary  
    # calculate here as it is used repeatedly and fixed for the simple case
    ipp1 = NULL; for ( v in 1:np ) ipp1 = c( ipp1, v )   # for unary processes 
    ipp2 = NULL; for ( v in 1:np ) ipp2 = c( ipp2, v,v ) # for binary processes
    
    while(res$simtime <= t.end ) {
      # browser()
      prop = res$P[]/res$P.total     # propensity
      j = sample( nP, size=1, replace=FALSE, prob=prop ) # index selection
      # remap random element to correct location and process
      jn = floor( (j-1)/nrc ) + 1  # which reaction process
      jj =  j - (jn-1)*nrc         # which cell 
      
      # determine focal cell coords
      cc = floor( (jj-1)/nr ) + 1   # col
      cr = jj - (cc-1) * nr         # row
      ro = cr + NU[1,,jn]  # row of the focal cell(s)
      co = cc + NU[2,,jn]  # column of the focal cell
      op = NU[3,,jn]
      
      ipp = ipp2  # default .. most frequent 
      mm = which( op == 0 ) 
      if (length(mm) > 0 ) { # switch to unary process
        op = op[-nn]
        ro = ro[-nn]
        co = co[-nn]
        ipp = ipp1  
      }

      # ensure boundary conditions are sane (reflective boundary conditions)
      ro[ro < 1] = 2
      ro[ro > nr] = nr_1
      co[co < 1] = 2
      co[co > nc] = nc_1 

      # determine the appropriate operations for the reaction and their 'real' locations
      # build correctly structured indices 
      ## row, column in X
      ix = cbind( ro, co)  
      ip = cbind( ro, co, ipp )   

      # update state space and associated propensities in cells where state has changed, etc
      res$X[ix] = XX = pmax( 0, res$X[ix] + op )
      P0 = res$P[ip]
      res$P[ip] = P1 = RE( p, X=XX, ix=ix ) 
  
      res$P.total = res$P.total + sum( P1 ) - sum(P0)
      res$simtime = res$simtime - (1/res$P.total) * log( runif(1) ) 
      ## this is the exponential deviate
      ## exp ~ -ln(1-runif)/lambda 
      ## but as 1-runif is also runif ==> -ln(runif())/lambda 
      ## -- ziggurat is faster ... willl add this
      res$nevaluations = res$nevaluations + 1
     
 

      if (res$simtime > tout) {
        tout = tout + t.censusinterval 
        tio = tio + 1  # time as index
        ssa.db( ptype="save", out=res$X[], tio=tio, outdir=outdir, rn=rn )  
        # global update of P in case of numerical drift
        res$P.total = sum(res$P[]) # reset P.total in case of divergence due to floating point errors
        if (monitor) {
          # print( P.total - sum(P[]) )  # debug
          # res$P = RE( p, res$X ) # redo all propensity estimates in case of numerical drift
          cat( paste( tio, res$nevaluations, round(sum(res$X)), round(res$P.total), Sys.time(), sep="\t\t" ), "\n" )
          image( res$X[], col=heat.colors(100)  )
          assign( "res", res, pos=1 ) # update the res output in the gloabl environment in case a restart is necessary -- might not be necessary as on.exit is doing the same thing
        }
      }
    } # end repeat
    return(res)
  })
  return(res)
}



