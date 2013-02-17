
  habitat.model.db = function( ip=NULL, DS=NULL, v=NULL, p=NULL, predictionYears=NULL, debug=F ) {
   
    # ~ 5hr , when k=200
    # variograms are not used .. the model solutions require > 3 days to complete! 
    # if GAMM speeds improve try again
    # currently fast GAM == "bam" is used
    if (!is.null(p$env.init)) for( i in p$env.init ) source (i)

    if ( DS %in% c("habitat.redo", "habitat" ) ) {
      
      outdir = file.path( project.directory("snowcrab"), "R", "gam", "models", "habitat" )
      dir.create(path=outdir, recursive=T, showWarnings=F)

      if( DS=="habitat") {
        Q = NULL
                
        # ****************************************
        # Parameterize habitat space models for various classes of crab rather than for every class 
        # (at least until we have more data) The following is what would need to change if 
        # this ever comes to pass
        # ****************************************

        v = habitat.template.lookup( v )   # <------

        # ****************************************
        
        fn = file.path( outdir, paste("habitat", v, "rdata", sep=".") )
        if (file.exists(fn)) load(fn)
        return(Q)
      }

      require(fields)
      require(mgcv)
      
      if (is.null(p$optimizers) ) p$optimizers = c("nlm", "bfgs", "perf", "bam", "newton", "optim", "nlm.fd")
      if (is.null(ip)) ip = 1:p$nruns

      for ( iip in ip ) {
        v0 = v = p$runs[iip,"v"]
        if ( v0 =="R0.mass.environmentals.only" ) v="R0.mass"

        fn = file.path( outdir, paste("habitat", v0, "rdata", sep=".") )
        set = snowcrab.db( DS="set.logbook" )
        set$total.landings.scaled = scale( set$total.landings, center=T, scale=T )
        
        set = presence.absence( set, v, p$habitat.threshold.quantile )  # determine presence absence (Y) and weighting(wt)

        tokeep=  c( "Y", "yr",  "julian", "plon", "plat", "t", "tmean", "tmean.cl", 
            "tamp", "wmin", "z", "substrate.mean", "dZ", "ddZ", "wt",
            "pca1", "pca2", "ca1", "ca2", "mr", "smr", "C", "Z", "sar.rsq", "Npred" ) 
        set = set[ , tokeep ]
        n0 = nrow(set)

        depthrange = range( set$z, na.rm= T) 

        if ( grepl("R0.mass", v) ) {   
          # add groundfish data
          gf = snowcrab.external.db (p=p, DS="set.snowcrab.in.groundfish.survey", vname=v )
          # absense prior to 1999 is meaningless due to inconsistent recording
          ii = which( gf$n==0 & gf$yr<=1998)
          if (length(ii)>0) gf = gf[-ii,]

          ii = which( gf$z < log(50) ) # drop strange data
          if (length(ii)>0) gf = gf[-ii,]

          ii = which( gf$z > log(650) ) # drop strange data
          if (length(ii)>0) gf = gf[-ii,]

          gf = presence.absence( gf, "n", p$habitat.threshold.quantile )  # determine presence absence and weighting  

          set = rbind( set, gf[, names(set)] )
         
          # add commerical fishery data
          lgbk = logbook.db( DS="fisheries.complete", p=p )
          lgbk = lgbk[ which( is.finite( lgbk$landings)), ]

          lgbk = presence.absence( lgbk, "landings", p$habitat.threshold.quantile )  # determine presence absence and weighting  
         
          baddata = which( lgbk$z < log(50) | lgbk$z > log(600) )
          if ( length(baddata) > 0 ) lgbk = lgbk[ -baddata,]
   
          lgbk$julian = convert.datecodes( lgbk$date.landed, "julian" )
          # lgbk$total.landings.scaled = scale( lgbk$landings, center=T, scale=T )
          lgbk = lgbk[, names(set)]
          set = rbind( set, lgbk )
        
          Z = bathymetry.db( DS="baseline", p=p ) 
          Z$plon = floor(Z$plon / 10)*10 
          Z$plat = floor(Z$plat / 10)*10 
          Z = Z[, c("plon", "plat") ]
          ii = which(duplicated(Z))
          if (length(ii)>0) Z = Z[-ii,] # thinned list of locations
          
          dd = rdist( set[,c("plon", "plat")] , Z )
          ee = apply( dd, 1, min, na.rm=T ) 
          ff = which( ee < p$threshold.distance ) # all within XX km of a  good data point
          set = set[ ff, ]
        }

        set$weekno = floor(set$julian / 365 * 52) + 1

        set$plon = jitter(set$plon)
        set$plat = jitter(set$plat)  
        
        set = set[ which(set$yr %in% p$years.to.model ) , ]
        set = set[ which (is.finite(set$Y + set$t + set$plon + set$z)) ,]
        
        set$dt.seasonal = set$tmean -  set$t 
        set$dt.annual = set$tmean - set$tmean.cl

        # remove extremes where variance is high due to small n
        set = filter.independent.variables( x=set )
 
        Q = NULL

        for ( o in p$optimizers ) {
          print (o )
          print( Sys.time() )
          
          .model = model.formula( v0 )

          ops = c( "outer", o ) 
          if (o=="perf") ops=o
          if (o=="bam") {
            Q = try( 
              bam( .model, data=set, weights=wt, family=binomial() ) ,
              silent=T
            )
          } else {
            Q = try( 
              gam( .model, data=set, optimizer=ops, weights=wt, family=binomial(), select=T )
              , silent=T
            )
          }

          if ( ! ("try-error" %in% class(Q) ) ) break()  # take the first successful solution
        }
        
        if ( "try-error" %in% class(Q) ) {
          print( paste( "No solutions found for:", v0 ) )
          next()
        
        }
        print (fn )

        save( Q, file=fn, compress=T )
        print( summary(Q) )
        print( fn )
        print( Sys.time() )
        
        debug = F
        if (debug) {
          summary(Q); AIC (Q) #207506
          ppp = predict( Q, set, type="response"); cor(ppp,set$Y, use="pairwise.complete.obs")^2 #.54
          require (boot)
          plot(Q, all.terms=T, rug=T, jit=T, seWithMean=T, pers=T, trans=inv.logit, scale=0 )
        }
      }

      return ( "Complete"  )  

    } # end if habitat


    # ---------------------


    if ( DS %in% c("abundance.redo", "abundance" ) ) {
      
      outdir = file.path( project.directory("snowcrab"), "R", "gam", "models", "abundance"  )
      dir.create(path=outdir, recursive=T, showWarnings=F)
      
      if( DS=="abundance") {
        Q = NULL
        fn = file.path( outdir, paste("abundance", v, "rdata", sep=".") )
        if (file.exists(fn)) load(fn)
        return(Q)
      }
            
      if (is.null(p$optimizers) ) p$optimizers = c( "nlm", "perf", "bfgs", "newton", "optim", "nlm.fd")
      if (is.null(ip)) ip = 1:p$nruns
   
      require(mgcv)
      require(boot)  

      for ( iip in ip ) {
        v = p$runs[iip, "v"]
        fn = file.path( outdir, paste("abundance", v, "rdata", sep=".") )
        set = snowcrab.db( DS="set.logbook" )
        set$Y = set[, v]
        set = set[ which(set$yr %in% p$years.to.model ) , ]
        set = set[ which (is.finite(set$Y + set$t + set$plon + set$z)) ,]
        
        set$total.landings.scaled = scale( set$total.landings, center=T, scale=T )
        set$sa.scaled =rescale.min.max(set$sa)
        set$sa.scaled[ which(set$sa.scaled==0)] = min (set$sa.scaled[ which(set$sa.scaled>0)], na.rm=T) / 2
     
        set$plon = jitter(set$plon)
        set$plat = jitter(set$plat)  
        
        set$weekno = floor(set$julian / 365 * 52) + 1
        set$dt.seasonal = set$tmean -  set$t 
        set$dt.annual = set$tmean - set$tmean.cl

        set$wgts = ceiling( set$sa.scaled * 1000 )

        # set zero values to a low number to be informative as this is in log-space
        iii = which(set$Y == 0 )
        if (length(iii)>0 ){
          # set = set[ - iii, ]
          set$Y[iii] = min( set$Y[ which(set$Y>0) ], na.rm=T ) / 100
        }
        set = set[ which( is.finite(set$Y + set$tmean + set$plon + set$z + set$wgts + set$weekno ) ) ,]
       
        set = set[ , c( "Y", "yr", "weekno", "plon", "plat", "tmean", "dt.annual", "dt.seasonal", 
            "tamp", "wmin", 
             "z",  "dZ", "substrate.mean", "wgts", "ca1", "ca2", "Npred", "Z", "smr", "mr"  ) ]
 
        # remove extremes where variance is high due to small n
        set = filter.independent.variables( x=set )

        print( summary(set))
        
        Q = NULL

        for ( o in p$optimizers ) {
          print (o )
          print( Sys.time() )
          ops = c( "outer", o ) 
          if (o=="perf") ops=o
          
          p$use.variogram.method = F
          if ( p$use.variogram.method) {

            Vrange = NULL
            Vpsill = NULL
            Zannual = NULL
            
            for ( iy in sort(unique(set$yr)) ) {
              ii = which(set$yr==iy)
              Z = var( set$Y[ii], na.rm=T )
              V = variogram( Y ~ 1, locations=~plon + plat, data=set[ii ,] , cutoff=100, 
                  boundaries=c(10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 100 ) , cressie=T)
              V$gamma = V$gamma / Z  # normalise it
              vf = fit.variogram(V, vgm(psill=0.5, model="Sph", range=50, nugget=0.5 ))
              plot( V, model=vf )
              Vrange = c( Vrange, vf$range[2] )
              Vpsill = c( Vpsill, vf$psill[2] / (vf$psill[1] + vf$psill[2] ) )
              Zannual = c( Zannual, Z )

            }
            Vrange = mean(Vrange[which(Vrange< 200)] , na.rm=T) 
            Vpsill = mean(Vpsill[which(Vrange< 200)], na.rm=T)
          
            Q = try( 
              gamm( model.formula (v ), data=set, optimizer=ops, weights=wgts,
                correlation=corSpher(c( Vrange, Vpsill ), form=~plon+plat | yr, nugget=T) ,
                family=gaussian(link = "log"),
              silent = T)
            )

          } else {
         
            Q = try( 
              gam(  model.formula (v ), data=set, optimizer=ops, weights=wgts, family=gaussian(link = "log"), select=T), silent = T )
           
          }
          if ( ! ("try-error" %in% class(Q) ) ) break()
        }
          
        if ( "try-error" %in% class(Q) ) {
          # last attempt with a simplified model
          Q = try( 
              gam(  model.formula ("simple" ), data=set, optimizer=ops, weights=wgts, family=gaussian(link = "log"), select=T), silent = T )
          if ( "try-error" %in% class(Q) ) {
            print( paste( "No solutions found for:", v ) )
            next()
          }
        }
        
        save( Q, file=fn, compress=T )
        print( summary(Q) )
        print( fn )
        print( Sys.time() )
        
        if (debug) {
          require (boot)
          AIC (Q) 
          summary(Q)
          ppp = predict( Q, set, type="response" )
          cor(ppp, set$Y, use="pairwise.complete.obs")^2 
          plot(Q, all.terms=T, rug=T, jit=T, seWithMean=F, pers=T, trans=exp, scale=0 )
        }

      }
      
      return ( "Complete"  )  

    } # end if
              
  } # end function

  # --------------


