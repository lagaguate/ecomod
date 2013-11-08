
 ssa.engine.parallel.bigmemory = function( p ) { 

  p <- within (p, { 
               
    simtime = tio = tout = nevaluations = 0
                  
    cl = makeCluster( spec=cluster, type=cluster.message.system )
    ssplt = lapply( clusterSplit( cl, 1:nsimultaneous.picks ), function(i){i} )

    while( simtime <= t.end ) {
     
      prop = .Internal(pmax(na.rm=FALSE, 0, P[]/P.total ) )
      J = .Internal(sample( nP, size=nsimultaneous.picks, replace=FALSE, prob=prop ) ) 
      time.increment = -(1/P.total)*log(runif( nsimultaneous.picks ) ) 
      
      psums = clusterApplyLB( cl=cl, x=ssplt, J=J, p=p, 
        
        fun = function( ip=NULL, J, p ) { 
        
          require(bigmemory)

          p <- within( p, {  # watch out ... a nested p ... only changing p$ppp

            X <- attach.big.matrix( bm.X )
            P <- attach.big.matrix( bm.P )

            if (is.null(ip)) ip =1:length(J)

            # ip = as.numeric(ip) 
            ppp = list()

            for ( iip in ip ) { 
              j = J[iip]
             
              # remap random index to correct location and process
              jn  = floor( (j-1)/nrc ) + 1  # which reaction process
              jj = j - (jn-1)*nrc  # which cell 
              cc = floor( (jj-1)/nr ) + 1
              cr = jj - (cc-1) * nr 
              
              # determine the appropriate operations for the reaction
              o = NU[[jn]] 
              no = dim(o)[1]
               
              # id coord(s) of reaction process(es) and ensure boundary conditions are sane
              ro = cr + o[,1] 
              co = cc + o[,2]
              ro[ro < 1] = 2
              ro[ro > nr] = nr_1
              co[co < 1] = 2
              co[co > nc] = nc_1 

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
                dP = RE( Xcx, b, d, K, DaR, DaC  )
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
        ssa.db( ptype="save", out=as.matrix(X[]), tio=tio )  
        # print( P.total - sum(P[]) )
        P.total = sum(P[]) # reset P.total to prevent divergence due to floating point errors
        cat( paste( tio, round(P.total), round(sum(X[])), nevaluations, Sys.time(), sep="\t\t" ), "\n" )
        # image( X[], col=heat.colors(100)  )
      }

    } # end repeat
    
    stopCluster( cl )

  })

  return (p )

 }


