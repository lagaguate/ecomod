
ssa.engine.approximation.cpp = function( p, res ) {
  # optimized and some minor approximations 
  
  res <- with (p, { 
    
    tio = tout = 0  # internal time counters to trigger data output (disk saves)
    on.exit( return(res) )  # in case we need to restart the sim with the last run
    #  on.exit( browser())   # to debug
   
    while (res$simtime <= t.end )  {
      # pre-caluclate these factor outside of the loop as they change slowly
      rns =  matrix( runif ( 2*nsimultaneous.picks ), ncol=2)
      
      # choose reaction times
      tn = insp  ## created as a vector in list p to speed it up a little
      nt = nsimultaneous.picks
      time.increment = -log( rns[,1] )/res$P.total 
      tnew = res$simtime + sum( time.increment )
      if ( tnew > tout ) {
        tn = which(  cumsum( time.increment ) <= tout ) 
        nt = length (tn)
        time.increment = time.increment[ tn ]
        tnew = res$simtime + sum( time.increment )
      }
      # choose reactions
      prop = cumsum( res$P[]/res$P.total )
      J = ssa_sample_direct( prop, sort( rns[,2] ) )
   
      # remap random element to correct location and process
      jn  = floor( (J-1)/nrc ) + 1  # which reaction process
      jj = J - (jn-1)*nrc  # which cell 
      # determine focal cell coords
      cc = floor( (jj-1)/nr ) + 1   # col
      cr = jj - (cc-1) * nr         # row
     
      if (length(tn) > 0 ) {
        for ( w in tn ) {
          o = NU[[ jn[w] ]]  
          # no = dim(o)[1]  # nrows = # operations (ie, unary, or binary)
          ro = cr[w] + o[,1]  # row of the focal cell
          co = cc[w] + o[,2]  # column of the focal cell
          # OP = o[,3] # operation upon the state variable
          # ensure boundary conditions are sane (reflective boundary conditions)
          ro[ro < 1] = 2
          ro[ro > nr] = nr_1
          co[co < 1] = 2
          co[co > nc] = nc_1 
          # determine the appropriate operations for the reaction and their 'real' locations
          # build correctly structured indices 
          ix = .Internal( cbind( deparse.level=1, ro, co))   ## row, column in X .. Internal speeds up 26%
          ip = .Internal( cbind( deparse.level=1, ro, co, po[[length(ro)]] ) )  # rows and columns in P 
          # update state space and associated propensities in cells where state has changed, etc
          
          res$X[ix] = XX = .Internal( pmax( na.rm=FALSE, 0, res$X[ix] + o[,3] ))
          res$P[ip] = RE( p, X=XX, ix=ix ) 
        }  # end for

        res$P.total = sum( res$P )
        res$simtime = tnew
        res$nevaluations = res$nevaluations + nt
      }

      if ( tnew >= tout ) {
        tout = tout + t.censusinterval 
        tio = tio + 1  # time as index
        # print( P.total - sum(P[]) ) # debug
        ssa.db( ptype="save", out=res$X[], tio=tio, outdir=outdir, rn=rn )  
        # res$P.total = sum(res$P[]) # reset P.total in case of divergence due to floating point errors
        # browser()
        if (monitor) {
          # res$P = RE( p, res$X ) # full refresh of propensities in case of numerical drift
          cat( paste( tio, res$nevaluations, round(sum(res$X)), round(res$P.total), Sys.time(), sep="\t\t" ), "\n" )
          image( res$X[], col=heat.colors(100)  )
          assign( "res", res, pos=1 ) # update the res output in the gloabl environment in case a restart is necessary
        }
      }

    } # end while 
    return(res)
  })  # end repeat and within
  return(res)
}


