
ssa.engine.approximation = function( p, res ) {
  # optimized and some minor approximations 
  res <- with (p, { 
    if (monitor) {
      on.exit( return(res) )  # in case we need to restart the sim with the last run
    } else {
      on.exit( browser())   # to debug
    }
    tio = tout = 0  # internal time counters to trigger data output (disk saves)
    while (res$simtime <= t.end )  {
      # pre-caluclate these factor outside of the loop as they change slowly
      # prop = .Internal(pmax(na.rm=FALSE, 0, res$P[]/res$P.total ) )
      prop = res$P[]/res$P.total
      J = .Internal(sample( nP, size=nsimultaneous.picks, replace=FALSE, prob=prop ) ) 
      time.increment = -(1/res$P.total)*log( runif ( nsimultaneous.picks ) ) 
      # remap random element to correct location and process
      jn  = floor( (J-1)/nrc ) + 1  # which reaction process
      jj = J - (jn-1)*nrc  # which cell 
      # determine focal cell coords
      cc = floor( (jj-1)/nr ) + 1   # col
      cr = jj - (cc-1) * nr         # row
      for ( w in 1:nsimultaneous.picks ) {
        o = NU[[ jn[w] ]]  
        no = dim(o)[1]  # nrows = # operations (ie, unary, or binary)
        ro = cr[w] + o[,1]  # row of the focal cell
        co = cc[w] + o[,2]  # column of the focal cell
        oper = o[,3] # operation upon the state variable
        # ensure boundary conditions are sane (reflective boundary conditions)
        ro[ro < 1] = 2
        ro[ro > nr] = nr_1
        co[co < 1] = 2
        co[co > nc] = nc_1 
        # determine the appropriate operations for the reaction and their 'real' locations
        # build correctly structured indices 
        ix = cbind( ro, co)  ## row, column in X
        ip = cbind( ro, co, po[[no]] )   # rows and columns in P 
        # update state space and associated propensities in cells where state has changed, etc
        res = RE( p, res, oper, ix, ip ) 
        res$simtime = res$simtime  + time.increment[w]   
        res$nevaluations = res$nevaluations + 1
        if (res$simtime > tout ) {
          tout = tout + t.censusinterval 
          tio = tio + 1  # time as index
          # print( P.total - sum(P[]) ) # debug
          ssa.db( ptype="save", out=res$X[], tio=tio, outdir=outdir, rn=rn )  
          res$P.total = sum(res$P[]) # reset P.total in case of divergence due to floating point errors
          if (monitor) {
            res$P = RE0( p, res$X ) # full refresh of propensities in case of numerical drift
            cat( paste( tio, res$nevaluations, round(sum(res$X)), round(res$P.total), Sys.time(), sep="\t\t" ), "\n" )
            image( res$X[], col=heat.colors(100)  )
            assign( "res", res, pos=1 ) # update the res output in the gloabl environment in case a restart is necessary
          }
        }
      }  # end for
    } # end while 
    return(res)
  })  # end repeat and within
  return(res)
}


