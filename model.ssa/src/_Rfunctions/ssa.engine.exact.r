
ssa.engine.exact = function( p) {

  p <- within (p, { 
               
    dir.create( outdir, recursive=TRUE, showWarnings=FALSE )
    
    simtime = tio = tout = nevaluations = 0
    
    while(simtime <= t.end ) {
      
      prop = .Internal(pmax(na.rm=FALSE, 0, P[]/P.total  ))   # using .Internal is not good syntax but this gives a major perfance boost > 40%
      j = .Internal(sample( nP, size=1, replace=FALSE, prob=prop ) )

      # remap random element to correct location and process
      jn  = floor( (j-1)/nrc ) + 1  # which reaction process
      jj = j - (jn-1)*nrc  # which cell 

      # focal cell coords
      cc = floor( (jj-1)/nr ) + 1
      cr = jj - (cc-1) * nr 

      # determine the appropriate operations for the reaction and their 'real' locations
      o = NU[[ jn ]]  
      no = dim(o)[1]
      
      ro = cr + o[,1] 
      co = cc + o[,2]
      
      # ensure boundary conditions are sane
      ro[ro < 1] = 2
      ro[ro > nr] = nr_1
      
      co[co < 1] = 2
      co[co > nc] = nc_1 
      
      # build correctly structured indices
      cox = cbind( ro, co)  ## in X
      cop = cbind( ro, co, po[[no]] )   # in P

      # update state (X) 
      Xcx = X[cox] + o[,3]   # Xcx is a temp copy to skip another lookup below
      Xcx[Xcx<0] = 0
      X[cox] = Xcx
  
      # update propensity in focal cells 
      dP = RE( Xcx, b, d, K, DaR, DaC )
      P.total = P.total + sum( dP - P[cop] )
      P[cop] = dP

      nevaluations = nevaluations + 1
      simtime = simtime - (1/P.total) * log( runif( 1))   # ... again to optimize for speed

      if (simtime > tout) {
        tout = tout + t.censusinterval 
        tio = tio + 1  # time as index
        # print( P.total - sum(P[]) )  # debug
        ssa.db( ptype="save", out=X[], tio=tio, outdir=outdir, rn=rn )  
        P.total = sum(P[]) # reset P.total in case of divergence due to floating point errors
        cat( paste( tio, round(P.total), round(sum(X)), nevaluations, Sys.time(), sep="\t\t" ), "\n" )
        image( X[], col=heat.colors(100)  )
      }
    } # end repeat
  })

  return(p)

}



