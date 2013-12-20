
ssa.engine.exact = function( p, res ) {

  res <- with (p, { 
    if (monitor) {
      on.exit( return(res) )  # in case we need to restart the sim with the last run
    } else {
      on.exit( browser())   # to debug
    }
    dir.create( outdir, recursive=TRUE, showWarnings=FALSE )
    tio = tout = 0
    while(res$simtime <= t.end ) {
      # NOTE: using .Internal is not good syntax but this gives a major perfance boost > 40% !
      prop = .Internal(pmax(na.rm=FALSE, 0, res$P[]/res$P.total  ))   # propensity
      j = .Internal(sample( nP, size=1, replace=FALSE, prob=prop ) ) # index selection
      # remap random element to correct location and process
      jn = floor( (j-1)/nrc ) + 1  # which reaction process
      jj =  j - (jn-1)*nrc         # which cell 
      # determine focal cell coords
      cc = floor( (jj-1)/nr ) + 1   # col
      cr = jj - (cc-1) * nr         # row
      o = NU[[ jn ]]  
      no = dim(o)[1]  # nrows = # operations (ie, unary, or binary)
      ro = cr + o[,1]  # row of the focal cell
      co = cc + o[,2]  # column of the focal cell
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
      res$nevaluations = res$nevaluations + 1
      res$simtime = res$simtime - (1/res$P.total) * log( runif(1) )
      if (res$simtime > tout) {
        tout = tout + t.censusinterval 
        tio = tio + 1  # time as index
        ssa.db( ptype="save", out=res$X[], tio=tio, outdir=outdir, rn=rn )  
        # global update of P in case of numerical drift
        res$P.total = sum(res$P[]) # reset P.total in case of divergence due to floating point errors
        if (monitor) {
          # print( P.total - sum(P[]) )  # debug
          res$P = RE0( p, res$X ) # redo all propensity estimates in case of numerical drift
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



