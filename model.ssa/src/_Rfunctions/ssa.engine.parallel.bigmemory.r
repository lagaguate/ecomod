
  # the bigmemeory approach is broken for now ... do not see utility

 ssa.engine.approximation.rcpp.parallel.bigmemory = function( p ) { 
  
  if ( pmethod=="rambacked.approximation.parallel" )  {
    # use parallel mode to run multiple simulations .. currently, this is the most efficient use of resources 
    p$libs = c( p$libs, loadlibraries(  "parallel", "bigmemory" ) )
    p$cluster = rep( "localhost", detectCores() )
    p$cluster.message.system = "SOCK" 
    
    p = ssa.db( p , DS="debug.big.matrix.rambacked" )
    p$monitor = FALSE
    b = ssa.engine.parallel.bigmemory( p  )
  }



  if (ssa.method == "filebacked.approximation.parallel" ) {
    p$libs = loadlibraries(  "parallel", "bigmemory" )
    p$cluster = rep( "localhost", detectCores() )
    # p$cluster = c( rep("tethys", 7), rep( "kaos", 23), rep("nyx", 24), rep( "tartarus", 24) ) 
    p$cluster.message.system = "SOCK" 
    p = ssa.db( p , DS="debug.big.matr:ix.filebacked" )
    p$monitor = FALSE
    p = ssa.engine.parallel.bigmemory( p )
  }


  loadfunctions( "model.ssa", filepattern="*\\.rcpp$" )  # load and compile supporting C/Rcpp programs  
  ## -- NOTE each parallel process needs it own copy of the Rcpp functions .. 
  ##         better to make into a package for a quick load in future.
  
  p <- within (p, { 
               
    simtime = tio = tout = nevaluations = 0
                  
    cl = makeCluster( spec=cluster, type=cluster.message.system )
    ssplt = lapply( clusterSplit( cl, 1:nsimultaneous.picks ), function(i){i} )

    while( simtime <= t.end ) {
        
        tn = tn0
        time.increment = random_deviate_exponential( nsimultaneous.picks, res$P.total) # rcpp function to sample from an exponential distrib
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
          J = random_deviate_uniform_weighted( nsimultaneous.picks, probs )  # rcpp function to sample from a weighted uniform dist.
     
          ri = reaction_location_indices( J, NU, nr, nc ) # rcpp function to remap random element to correct location and process
          ix = cbind( ri$xr, ri$xc ) # rows and columns
          ip = cbind( ri$pr, ri$pc, ri$po ) 
        } 

      psums = clusterApplyLB( cl=cl, x=ssplt, J=J, p=p, 
        
        fun = function( ip=NULL, J, p ) { 
        
          require(bigmemory)

          p <- with( p, {  # watch out ... a nested p ... only changing p$ppp

            X <- attach.big.matrix( bm.X )
            P <- attach.big.matrix( bm.P )

            if (is.null(ip)) ip =1:length(J)

            # ip = as.numeric(ip) 
            ppp = list()

            for ( iip in ip ) { 
              
              j = 
              # update state (X) 
              Pchange = 0
              for ( l in 1:no ) {
                rr = ro[l]
                cc = co[l]
                
                Xcx = X[ rr, cc] + o[l,3]    ## bigmatrix uses a different refercing system than matrix
                Xcx[ Xcx<0 ] = 0
                X[rr,cc] = Xcx
              
                # update propensity in focal and neigbouring cells 
                jcP = cc + c(0: (np-1))*nc  # increment col indices depending upon reaction process
                dP = RE( p, X, cox )
                Pchange = Pchange + sum( dP - P[ rr, jcP ] )
                P[rr,jcP ] = dP
              }

              ppp[[iip]] = Pchange
            }

        })
        
        return(p$ppp) 

      }) # end clusterapply

      P.total = P.total + sum( .Internal(unlist( psums, TRUE, FALSE )) )
      nevaluations = nevaluations + nsimultaneous.picks
      simtime = simtime + sum(time.increment)   
        
      if (simtime > tout ) {
            
        X <- attach.big.matrix( bm.X, path=outdir)   ### might need to remove the path=outdir if using RAM-backed bigmemory object
        P <- attach.big.matrix( bm.P, path=outdir)

        tout = tout + t.censusinterval 
        tio = tio + 1  # time as index
        ssa.db( p=p, DS="save", out=as.matrix(X[]), tio=tio )  
        # print( P.total - sum(P[]) )
        P.total = sum(P[]) # reset P.total to prevent divergence due to floating point errors
        if (monitor) {
          P = array( RE( p, X, 1:nrc), dim=c( nr, nc, np ) )
          cat( paste( tio, round(P.total), round(sum(X[])), nevaluations, Sys.time(), sep="\t\t" ), "\n" )
          image( X[], col=heat.colors(100)  )
        }
      }

    } # end repeat
    
    stopCluster( cl )

  })

  return (p )

 }


