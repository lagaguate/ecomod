
ssa.engine.approximation.rcpp = function( p, res ) {
  # optimized and some minor approximations 
  
  res <- with (p, { 
       
    on.exit( return(res) )  # in case we need to restart the sim with the last run
    tio = tout = 0  # internal time counters to trigger data output (disk saves)
    ip1 = 1:np  # unary indices
    ip2 = NULL; for ( v in 1:np ) ip2 = c( ip2, v, v )  # binary indices
    tn0 = 1:nsimultaneous.picks

    while ( res$simtime <= t.end )  {

      tn = tn0
      time.increment = random_deviate_exponential_rcpp( nsimultaneous.picks, res$P.total)
      tnew = res$simtime + sum( time.increment )
      
      if ( tnew > tout ) {
        tcs = cumsum( time.increment )
        tn = which( tcs <= tout ) 
        time.increment = time.increment[ tn ]
        tnew = res$simtime + sum( time.increment )
      }

      tnlen =length(tn)

      if ( tnlen > 0 ) {
        probs = res$P[]/res$P.total
        J = random_deviate_uniform_weighted_rcpp( nsimultaneous.picks, probs )  
        # remap random element to correct location and process 
        # the following uses a common C-algorthm that uses minimal number of computations
        # C uses indices that begin at 0 so subtract 1 firt (J ranges from 1 ...) 
        # %/% is integer division; %% is modulus ~ 5% faster than using direct older index method .. but could be statistical fluctuation too
        J = J - 1 # to get C-index starting at 0;; 
        cr =  J %% nr + 1;           #    -- row no , the +1's are because R indices begin at 1 not 0 as in C
        cc = (J%/%nr) %% nc + 1;       #    -- col no
        jn =  J %/% nrc + 1;             # -- processes np
            
        ro = cr + t(NU[1,,jn])  # row of the focal cell(s)
        co = cc + t(NU[2,,jn])  # column of the focal cell
        op = t(NU[3,,jn])     # operations upon each offset

        # ensure boundary conditions are sane (reflective boundary conditions)
        ro[ro < 1] = 2
        ro[ro > nr] = nr_1
        co[co < 1] = 2
        co[co > nc] = nc_1 
        
        # update state space and associated propensities in cells where state has changed, etc
        # determine the appropriate operations for the reaction and their 'real' locations
        # build correctly structured indices 
        # res$X[ix] = XX = .Internal(pmax( na.rm=FALSE, 0, res$X[ix] + op ) )
           
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
        res$P.total = res$P.total + Ptot_delta  
        res$simtime = tnew
        res$nevaluations = res$nevaluations + tnlen
      }

      if ( tnew >= tout ) {
        tout = tout + t.censusinterval 
        tio = tio + 1  # time as index
        ssa.db( ptype="save", out=res$X[], tio=tio, outdir=outdir, rn=rn )  
        if (monitor) {
          # res$P = RE( p, res$X ) # full refresh of propensities in case of numerical drift
          cat( paste( tio, res$nevaluations, round(sum(res$X)), round(res$P.total), Sys.time(), sep="\t\t" ), "\n" )
          image( res$X[], col=heat.colors(100)  )
          # assign( "res", res, pos=1 ) # update the res output in the gloabl environment in case a restart is necessary
        }
      }

    } # end while 
    return(res)
  })  # end repeat and within
  return(res)
}


