
ssa.engine.approximation = function( p, res ) {
  
  # optimized a few steps at the cost of a few approximations relating to simultaneity of processes
  # approximation simular to the tau-leaping method:: ideally only one process should be picked at a time ... 
  # sampling from the propensities is time-expensive, so a number of picks are made in advance and then updated ..

  res <- with (p, { 
    
    #  on.exit( browser())   # to debug
    on.exit( return(res) )  # in case we need to restart the sim with the last run
    tio = tout = 0  # internal time counters to trigger data output (disk saves)
    ip1 = 1:np  # unary indices
    ip2 = NULL; for ( v in 1:np ) ip2 = c( ip2, v, v )  # binary indices
      
    nsimultaneous.picks = round( p$nrc * ssa.approx.proportion )
    tn0 = 1:nsimultaneous.picks

    while (res$simtime <= t.end )  {
      # pre-caluclate these factor outside of the loop as they change slowly
     
      tn = tn0  
      time.increment = -(1/res$P.total)*log( runif ( nsimultaneous.picks ) ) # R only -- slow
      tnew = res$simtime + sum( time.increment )
      
      if ( tnew > tout ) {
        tcs = cumsum( time.increment )
        tn = which( tcs <= tout ) 
        time.increment = time.increment[ tn ]
        tnew = res$simtime + sum( time.increment )
      }

      tnlen =length(tn)

      if (tnlen > 0 ) {
        prop = res$P[]/res$P.total
        J = sample( nP, size=nsimultaneous.picks, replace=FALSE, prob=prop ) 
        # remap random element to correct location and process 
        # the following uses a common C-algorthm that uses minimal number of computations
        # C uses indices that begin at 0 so subtract 1 firt (J ranges from 1 ...) 
        # %/% is integer division; %% is modulus ~ 5% faster than using direct older index method .. but could be statistical fluctuation too
        j0 = J-1  # above math applies to 0-indexing 
        jn = j0 %/% nrc   # which reaction process  %/% is integer division 
        jj = j0 - jn*nrc   # which cell  
        cc = jj %/% nr    # col
        cr = jj - cc*nr  # row
        
        ## ---- above was using 0-indexing now must add 1 to return to R-indexing
        jn = jn + 1 
        cr = cr + 1
        cc = cc + 1

        # apply offsets to focal coord
        ro = cr + t(NU[1,,jn])  # row of the focal cell(s)
        co = cc + t(NU[2,,jn])  # column of the focal cell
        op = t(NU[3,,jn])     # operations upon each offset

        # ensure boundary conditions are sane (reflective boundary conditions)
        ro[ro < 1] = 2
        ro[ro > nr] = nr_1
        co[co < 1] = 2
        co[co > nc] = nc_1 
      
        Ptot_delta = 0
        for ( w in 1:tnlen ) {
          # determine the appropriate operations for the reaction and their 'real' locations
          # build correctly structured indices 
          op_w = op[w,]
          ro_w = ro[w,]
          co_w = co[w,]

          unary = which(op_w == 0)
          if (length( unary)>0 ) {
            op_w = op_w[-unary]
            ro_w = ro_w[-unary]
            co_w = co_w[-unary]
            ip = .Internal( cbind( deparse.level=1, ro_w, co_w, ip1))   ## row, column in X .. Internal speeds up 26%
          } else {
            ip = .Internal( cbind( deparse.level=1, ro_w, co_w, ip2))   ## row, column in X .. Internal speeds up 26%
          }
          
          ix = .Internal( cbind( deparse.level=1, ro_w, co_w ) )   ## row, column in X .. Internal speeds up 26%
          res$X[ix] = XX = .Internal( pmax( na.rm=FALSE, 0, res$X[ix] + op_w ) )
          
          P0 = sum( res$P[ip] )
          res$P[ip] = PP = RE( p, X=XX, ix=ix )
          Ptot_delta = Ptot_delta + sum(PP) - P0
        }
        
          
        # update state space and associated propensities in cells where state has changed, etc
        res$P.total = sum(res$P[]) 
        res$simtime = res$simtime + sum(time.increment)
        res$nevaluations = res$nevaluations + length(time.increment)
      }

      if ( tnew >= tout ) {
        tout = tout + t.censusinterval 
        tio = tio + 1  # time as index
        # print( P.total - sum(P[]) ) # debug
        res$P.total = sum(res$P)  # reset P.total in case of divergence due to floating point errors
        ssa.db( p=p, DS="save", out=res$X[], tio=tio )  
        # browser()
        if (monitor) {
          # res$P =array( RE( p, res$X ),  dim=c( nr, nc, np ) )
          # # full refresh of propensities in case of numerical drift
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


