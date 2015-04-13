
  habitat.model.db = function( ip=NULL, DS=NULL, v=NULL, p=NULL, yr=NULL, debug=F ) {

    # ~ 5hr , when k=200
    # variograms are not used .. the model solutions require > 3 days to complete! 
    # if GAMM speeds improve try again
    # currently fast GAM == "bam" is used
     
    if (DS %in% c("large.male.auxillary.data", "large.male.auxillary.data.redo") ) {
      
      outdir = project.datadirectory("snowcrab", "R"  )
      dir.create(path=outdir, recursive=T, showWarnings=F)
      fn = file.path( outdir, paste("habitat.model", "large_male_auxillary_data", "basedata", "rdata", sep=".") ) 
      
      set = NULL
      if ( DS %in% c("large.male.auxillary.data" )) {
        if (file.exists(fn)) load(fn)
        return( set)
      }

      # add groundfish data
      gf = snowcrab.external.db (p=p, DS="set.snowcrab.in.groundfish.survey", vname="R0.mass" )  ## right now, fixed only to R0.mass ... TO DO make more variable
      # absense prior to 1999 is meaningless due to inconsistent recording
      ii = which( gf$n==0 & gf$yr<=1998)
      if (length(ii)>0) gf = gf[-ii,]

      ii = which( gf$z < log(50) ) # drop strange data
      if (length(ii)>0) gf = gf[-ii,]

      ii = which( gf$z > log(650) ) # drop strange data
      if (length(ii)>0) gf = gf[-ii,]

      gf = presence.absence( X=gf, vname="n", px=p$habitat.threshold.quantile )  # determine presence absence and weighting  

      # add commerical fishery data
      lgbk = logbook.db( DS="fisheries.complete", p=p )
      lgbk = lgbk[ which( is.finite( lgbk$landings)), ]

      lgbk = presence.absence( X=lgbk, vname="landings", px=p$habitat.threshold.quantile )  # determine presence absence and weighting  
     
      baddata = which( lgbk$z < log(50) | lgbk$z > log(600) )
      if ( length(baddata) > 0 ) lgbk = lgbk[ -baddata,]

      lgbk$julian = convert.datecodes( lgbk$date.landed, "julian" )
      
      nms = intersect( names(gf) , names( lgbk) )
      set = rbind( gf[, nms], lgbk[,nms] )
   
      Z = bathymetry.db( DS="baseline", p=p ) 
      Z$plon = floor(Z$plon / 10)*10 
      Z$plat = floor(Z$plat / 10)*10 
      Z = Z[, c("plon", "plat") ]
      ii = which(duplicated(Z))
      if (length(ii)>0) Z = Z[-ii,] # thinned list of locations
      
      dd = rdist( set[,c("plon", "plat")] , Z )
      ee = apply( dd, 1, min, na.rm=T ) 
      ff = which( ee < p$threshold.distance ) # all within XX km of a good data point
      set = set[ ff, ]

      save ( set, file=fn, compress=TRUE )
      
      return (fn)
    }



    if ( DS %in% c("basedata" ) ) {
    
      set = snowcrab.db( DS="set.logbook" )
      set$total.landings.scaled = scale( set$total.landings, center=T, scale=T )
        
      set = presence.absence( X=set, vname=v, px=p$habitat.threshold.quantile )  # determine presence absence (Y) and weighting(wt)
      n0 = nrow(set)

      depthrange = range( set$z, na.rm= T) 

#      if ( grepl("R0.mass", v) ) {   
#        aset = habitat.model.db( DS="large.male.auxillary.data", p=p )
#        set = rbind( set, aset[, names(set)] )
#      }

      set$weekno = floor(set$julian / 365 * 52) + 1

      set$plon = jitter(set$plon)
      set$plat = jitter(set$plat)  
      
      # set = set[ which(set$yr %in% p$years.to.model ) , ]
      # set = set[ which (is.finite(set$Y + set$t + set$plon + set$wt)) ,]
      
      set$dt.seasonal = set$tmean -  set$t 
      set$dt.annual = set$tmean - set$tmean.cl
      
      # used for abundance modelling
      set$total.landings.scaled = scale( set$total.landings, center=T, scale=T )
      set$sa.scaled =rescale.min.max(set$sa)
      set$sa.scaled[ which(set$sa.scaled==0)] = min (set$sa.scaled[ which(set$sa.scaled>0)], na.rm=T) / 2
      #set$wgts = ceiling( set$sa.scaled * 1000 )
      set$wgts = 1

      # remove extremes where variance is high due to small n
      # set = filter.independent.variables( x=set )

      tokeep=  c( "Y", "yr",  "julian", "weekno", "dt.annual", "dt.seasonal", "plon", "plat", "wt", "wgts",  v, p$auxilliary.data ) 
      # tokeep = intersect( names(set), tokeep) 
      
      set = set[ , tokeep ]

      return(set)

    }

    
    if ( DS %in% c("habitat.redo", "habitat" ) ) {
         
      outdir =  project.datadirectory("snowcrab", "R", "gam", "models", "habitat" )
      dir.create(path=outdir, recursive=T, showWarnings=F)

      if( DS=="habitat") {
        Q = NULL
                
        # ****************************************
        # Parameterize habitat space models for various classes of crab rather than for every class 
        # (at least until we have more data) The following is what would need to change if 
        # this ever comes to pass
        # ****************************************

        # v = habitat.template.lookup( v )   # <------

        # ****************************************
        
        if(any(p$movingdatawindow!=0)) fn = file.path( outdir, paste("habitat", v, yr, "rdata", sep=".") )
        if(all(p$movingdatawindow==0)) fn = file.path( outdir, paste("habitat", v, "rdata", sep=".") )
        
        if (file.exists(fn)) load(fn)
        return(Q)
      }

      if (exists( "init.files", p)) {
        p0 = p; LoadFiles( p$init.files ) 
        p=p0
        }
      if (exists( "libs", p)) RLibrary( p$libs ) 
      if (is.null(ip)) ip = 1:p$nruns
      
      if (!exists( "optimizers", p) ) p$optimizers = c( "bam", "nlm", "bfgs", "perf", "newton", "optim", "nlm.fd")

      for ( iip in ip ) {
        v0 = v = p$runs[iip,"v"]
        if(any(p$movingdatawindow!=0)) yr = p$runs[iip,"yrs"]
        print ( p$runs[iip,] )
        if ( v0 =="R0.mass.environmentals.only" ) v="R0.mass"
        if(any(p$movingdatawindow!=0)) fn = file.path( outdir, paste("habitat", v0, yr, "rdata", sep=".") ) #to turn off moving window
        if(all(p$movingdatawindow==0)) fn = file.path( outdir, paste("habitat", v0, "rdata", sep=".") ) #to turn off moving window
        
        set = habitat.model.db( DS="basedata", p=p, v=v )
            
        if(any(p$movingdatawindow!=0)) yrsw = c( p$movingdatawindow + yr  ) #turn off moving window Feb 2015
        if(all(p$movingdatawindow==0))     yrsw = p$years.to.model
        ist = which( set$yr %in% yrsw ) # default year window centered on focal year
        nyrsw = length ( unique( set$yr[ ist ] ) )
        if ( nyrsw  < p$movingdatawindowyears ) {
          for ( ny in 1:5 ) {
            yrsw = c( (min(yrsw)-1), yrsw, (max(yrsw)+1) )
            ist = which( set$yr %in% yrsw ) # five year window
            yrs_selected = sort( unique( set$yr[ ist ] ) )
            nyrsw = length ( yrs_selected )
            if (nyrsw == p$movingdatawindowyears ) break() 
          }
        }
       
        if ( length(ist) < 30 ) {
            print( paste( "Insufficient data found for:", p$runs[iip,] ) )
          next()
        } 


        set = set[ ist , ]        
        
        
        Q = NULL
        .model = model.formula( v0 )

        ntest = all.vars(.model) %in% names(set)
        if ( !all(ntest) ) {
          print( "Error. Data elements in set might be missing relative to those expected in the model formula: " )
          print ( all.vars(.model)[  which(ntest)] )
          stop()
        }

        fmly = binomial()
        for ( o in p$optimizers ) {
          print (o )
          print( Sys.time() )
          
          
          ops = c( "outer", o ) 
          if (o=="perf") ops=o
          if (o=="bam") {
           # Q = try( bam( .model, data=set, weights=wt, family=fmly  ), silent=F )
            Q = try( bam( .model, data=set, family=fmly  ), silent=F )
           
          } else {
           # Q = try( gam( .model, data=set, weights=wt, family=fmly, select=T, optimizer=ops ), silent=F )
           Q = try( gam( .model, data=set, family=fmly, select=T, optimizer=ops ), silent=F )
          
          }
          
          print(Q)

          if ( ! ("try-error" %in% class(Q) ) ) {
            break()  # take the first successful solution
          }
        }
         
        # last resort
        if ( "try-error" %in% class(Q) ) {
          # last attempt with a simplified model and default optimizer
          .model = model.formula ("simple" )
         # Q = try( gam( .model, data=set,  weights=wt, family=fmly, select=T), silent = F )
          Q = try( gam( .model, data=set,  family=fmly, select=T), silent = F )
          print(Q)
         
          if ( "try-error" %in% class(Q) ) {
            print( paste( "No solutions found for:", v ) )
            next()
          }
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

      print ( "Complete"  )  
      return(Q)

    } # end if habitat


    # ---------------------


    if ( DS %in% c("abundance.redo", "abundance" ) ) {
      
      outdir = file.path( project.datadirectory("snowcrab"), "R", "gam", "models", "abundance"  )
      dir.create(path=outdir, recursive=T, showWarnings=F)
      
      if( DS=="abundance") {
        Q = NULL
        if(any(p$movingdatawindow!=0)) fn = file.path( outdir, paste("abundance", v, yr, "rdata", sep=".") )
        if(all(p$movingdatawindow==0)) fn = file.path( outdir, paste("abundance", v, "rdata", sep=".") )
        
        if (file.exists(fn)) load(fn)
        return(Q)
      }
 
      if (exists( "init.files", p)) LoadFiles( p$init.files ) 
      if (exists( "libs", p)) RLibrary( p$libs ) 

      if (is.null(p$optimizers) ) p$optimizers = c( "bam", "nlm", "perf", "bfgs", "newton", "optim", "nlm.fd")
      if (is.null(ip)) ip = 1:p$nruns

      for ( iip in ip ) {
        v = p$runs[iip, "v"]
        if(any(p$movingdatawindow!=0)) yr = p$runs[iip,"yrs"]
        print( p$runs[iip,])
  if(any(p$movingdatawindow!=0)) fn = file.path( outdir, paste("abundance", v, yr, "rdata", sep=".") )
  if(all(p$movingdatawindow==0)) fn = file.path( outdir, paste("abundance", v, "rdata", sep=".") )
        set = habitat.model.db( DS="basedata", p=p, v=v )
        # set = snowcrab.db( DS="set.logbook" )
        set$Y = set[, v]  # override -- Y is P/A
       
  if(any(p$movingdatawindow!=0))  yrsw = c( p$movingdatawindow + yr  )
  if(all(p$movingdatawindow==0))  yrsw = c( p$years.to.model)
        
        ist = which( set$yr %in% yrsw ) # default year window centered on focal year
        nyrsw = length ( unique( set$yr[ ist ] ) )
        if ( nyrsw  < p$movingdatawindowyears ) {
          for ( ny in 1:5 ) {
            yrsw = c( (min(yrsw)-1), yrsw, (max(yrsw)+1) )
            ist = which( set$yr %in% yrsw ) # five year window
            yrs_selected = sort( unique( set$yr[ ist ] ) )
            nyrsw = length ( yrs_selected )
            if (nyrsw == p$movingdatawindowyears ) break() 
          }
        }

        if ( length(ist) < 30 ) {
            print( paste( "Insufficient data found for:", p$runs[iip,] ) )
          next()
        } 
        set = set[ ist , ]        

        im = which (is.finite(set$Y + set$t + set$plon + set$z)) # impertive
        if ( length(im) < 30 ) {
            print( paste( "Insufficient data found for:", p$runs[iip,] ) )
          next()
        } 
        set = set[ im ,]  
        

        # set zero values to a low number to be informative as this is in log-space
        iii = which(set$Y == 0 )
        if (length(iii)>0 ){
          # set = set[ - iii, ]
          set$Y[iii] = min( set$Y[ which(set$Y>0) ], na.rm=T ) / 100   # 1 percentile 
        }
 
        # remove extremes where variance is high due to small n
        # set = filter.independent.variables( x=set )

        print( summary(set))
        
        Q = NULL
        .model = model.formula (v )
   
        ntest = all.vars(.model) %in% names(set)
        if ( !all(ntest) ) {
          print( "Error. Data elements in set might be missing relative to those expected in the model formula: " )
          print ( all.vars(.model)[  which(!ntest)] )
          stop()
        }


        fmly = gaussian(link = "log")

        for ( o in p$optimizers ) {
          print (o )
          print( Sys.time() )
         
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
                silent = F)
              )
            } 
 
          ops = c( "outer", o ) 
          if (o=="perf") ops=o
          if (o=="bam") {
            #Q = try(  bam( .model, data=set, weights=wgts, family=fmly ), silent=F )
            Q = try(  bam( .model, data=set, family=fmly ), silent=F )
         
           } else {
            #Q = try( gam( .model, data=set, optimizer=ops, weights=wgts, family=fmly, select=T ), silent = F )
            Q = try( gam( .model, data=set, optimizer=ops, family=fmly, select=T ), silent = F )
         
          }
          print(Q)
          if ( ! ("try-error" %in% class(Q) ) ) break()  # first good solution exits
        }
        
        # last resort
        if ( "try-error" %in% class(Q) ) {
          # last attempt with a simplified model
          .model = model.formula ("simple" )
          #Q = try( gam( .model, data=set, weights=wgts, family=fmly, select=T), silent = F )
          Q = try( gam( .model, data=set, family=fmly, select=T), silent = F )
         
          print(Q)
          if ( "try-error" %in% class(Q) ) {
            print( paste( "No solutions found for:", v ) )
            next()
          }
        }
        
        save( Q, file=fn, compress=T )
        print( summary(Q) )
        print( fn )
        print( Sys.time() )
        debug=F
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


