
ssa.engine.exact = function( p, res ) {

  res <- with (p, { 
    dir.create( outdir, recursive=TRUE, showWarnings=FALSE )
    tio = tout = 0
    while(res$simtime <= t.end ) {
      # NOTE: using .Internal is not good syntax but this gives a major perfance boost > 40% !
      prop = .Internal(pmax(na.rm=FALSE, 0, res$P[]/res$P.total  ))   # propensity
      j = .Internal(sample( nP, size=1, replace=FALSE, prob=prop ) ) # index selection
      # remap random element to correct location and process
      jn = floor( (j-1)/nrc ) + 1  # which reaction process
      jj =  j - (jn-1)*nrc         # which cell 
      # update state space and associated propensities in cells where state has changed, etc
      res = RE( p, res, jn, jj ) 
      # res$nevaluations = res$nevaluations + 1
      res$simtime = res$simtime - (1/res$P.total) * log( runif( 1))
      if (res$simtime > tout) {
        tout = tout + t.censusinterval 
        tio = tio + 1  # time as index
        ssa.db( ptype="save", out=res$X[], tio=tio, outdir=outdir, rn=rn )  
        # global update of P in case of numerical drift
        res$P.total = sum(res$P[]) # reset P.total in case of divergence due to floating point errors
        if (monitor) {
          # print( P.total - sum(P[]) )  # debug
          res$P = array( RE0( p, res$X ), dim=c( nr, nc, np ) )
          cat( paste( tio, round(res$P.total), round(sum(res$X)), Sys.time(), sep="\t\t" ), "\n" )
          image( res$X[], col=heat.colors(100)  )
          # browser()
          assign( "res", res, pos=1 ) # update the res output in the gloabl environment in case a restart is necessary
          debug=FALSE
          if (debug) {
            # print( res$nevaluations )
          }
        }
      }
    } # end repeat
    return(res)
  })
  return(res)
}



