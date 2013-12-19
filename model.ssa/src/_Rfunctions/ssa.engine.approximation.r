
ssa.engine.approximation = function( p, res ) {

  res <- with (p, { 
    on.exit( browser())   # to debug
    # on.exit( return(res) )  # in case we need to restart the sim with the last run
    tio = tout = 0  # internal time counters to trigger data output (disk saves)
    while (res$simtime <= t.end )  {
      # pre-caluclate these factor outside of the loop as they change slowly
      prop = .Internal(pmax(na.rm=FALSE, 0, res$P[]/res$P.total ) )
      J = .Internal(sample( nP, size=nsimultaneous.picks, replace=FALSE, prob=prop ) ) 
      time.increment = -(1/res$P.total)*log( runif ( nsimultaneous.picks ) ) 
      for ( w in 1:nsimultaneous.picks ) {
        j = J[w]
        # remap random element to correct location and process
        jn  = floor( (j-1)/nrc ) + 1  # which reaction process
        jj = j - (jn-1)*nrc  # which cell 
        # update state space and associated propensities in cells where state has changed, etc
        res = RE( p, res, jn, jj ) 
        res$simtime = res$simtime  + time.increment[w]   
        res$nevaluations = res$nevaluations + 1
        if (res$simtime > tout ) {
          tout = tout + t.censusinterval 
          tio = tio + 1  # time as index
          # print( P.total - sum(P[]) ) # debug
          ssa.db( ptype="save", out=res$X[], tio=tio, outdir=outdir, rn=rn )  
          res$P.total = sum(res$P[]) # reset P.total in case of divergence due to floating point errors
          if (monitor) {
            res$P = array( RE0( p, res$X ), dim=c( nr, nc, np ) )
            cat( paste( tio, round(res$P.total), round(sum(res$X)), Sys.time(), sep="\t\t" ), "\n" )
            image( res$X[], col=heat.colors(100)  )
            # browser()
            assign( "res", res, pos=1 ) # update the res output in the gloabl environment in case a restart is necessary
            debug=FALSE
            if (debug) {
              print( res$nevaluations )
            }
          }
        }
      }  # end for
    } # end while 
    return(res)
  })  # end repeat and within
  return(res)
}


