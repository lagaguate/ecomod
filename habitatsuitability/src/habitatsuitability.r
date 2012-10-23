
  # Habitat suitability estimation

	
	loadlibraries (c("chron", "fields", "rgdal", "snow", "mgcv", "arm" ))

	p = list()
  p$env.init = loadfunctions(c( "common", "habitat", "temperature",  "bathymetry"	)) 
  p = spatial.parameters( type="SSE" ) # 4VWX
  
  p$optimizers = c( "nlm", "perf" )  # used by GAM
  p$studyarea = c("4vwx", "5yz" )
  p$habitat.threshold.quantile = 0.05 # quantile at which to consider zero-valued abundance
  p$prediction.month = 10

  # --------------
  # do spatial predictions using GAM
  p$nsims = 1000

#  p$gam.model.pa = formula( presence ~  s( yr ) + s(month, k=3) 
#        + s( dt.seasonal ) + s( dt.annual ) + s( tmean.annual ) + s( tamp.annual) +  s( wmin.annual )
#        + s( plon, plat, k=400) + s( z ) + s( substrate.mean) + s( dZ ) + s(ddZ) 
#        + s( Npred) + s(smr)  + s(ca1) +s(ca2) 
#      )
  
#  p$gam.model.ra = formula( qn ~ s( yr )+ s(month, k=3)  
#        + s( dt.seasonal ) + s( dt.annual ) + s( tmean.annual )  + s( tamp.annual) + s( wmin.annual) 
#        + s( plon, plat, k=400) + s( z ) + s( substrate.mean)  +s( dZ) + s(ddZ)  
#        + s( Npred) + s(smr) + s(ca1) +s(ca2)  
#      )

  p$gam.model.pa = formula( presence ~  s( yr ) + s(month, k=3) 
        + s( tmean.annual ) + s( tamp.annual) +  s( wmin.annual )
        + s( plon, plat, k=400) + s( z ) + s( substrate.mean) + s( dZ ) + s(ddZ) 
        + s( Npred) + s(smr)  + s(ca1) +s(ca2) 
      )
  
  p$gam.model.ra = formula( qn ~ s( yr )+ s(month, k=3)  
        + s( tmean.annual )  + s( tamp.annual) + s( wmin.annual) 
        + s( plon, plat, k=400) + s( z ) + s( substrate.mean)  +s( dZ) + s(ddZ)  
        + s( Npred) + s(smr) + s(ca1) +s(ca2)  
      )


  p$yearstomodel = 1970:2011 # set map years separately to temporal.interpolation.redo allow control over specific years updated
 
  # choose:
  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep( "localhost", 2 )
  # p$clusters = rep( "localhost", 8 )
  # p$clusters = rep( "localhost", 24 )

  p$clusters = c( rep( "nyx.beowulf", 4), rep("tartarus.beowulf", 4), rep("kaos", 4 ) )


  speciesofinterest = c( "wolffish", "white.hake", "thornyskate", "american.plaice", "cod", "redfish" )
  
  # --------------
  # data

  if (refresh.databases) {
    for (sp in speciesofinterest ) {
      print (sp )
      p$speciesofinterest = sp
      habitatsuitability.generic.db( DS="complete.redo", p=p )
    }
  }




  #----------------------------
  # model presence and absence using GAM

  for ( sp in speciesofinterest)  {
  
    # sp = speciesofinterest = "wolffish"
    print(sp)
    p$speciesofinterest = sp
   
    model.presence = T
    if (model.presence) {
      # presence - absence  ~ 6hr & 16 GB
      predict.discretised.habitat( DS="model.pa.redo", p=p )    
      debug=F
      if ( debug ) {
        Q = predict.discretised.habitat( DS="model.pa", p=p )    
        summary(Q)
        require(boot)
        plot(Q, all.terms=T, rug=T, jit=T, seWithMean=T, trans=inv.logit, pers=F , scale=0)
        vis.gam( Q, view=c("plon", "plat"), plot.type="contour", n.grid=200, too.far=2 ) 
      }
    }


    model.abundance = T
    if (model.abundance) {
      # relative abundance :: 0.424 Rsq, 43.1% deviance explained 
      predict.discretised.habitat( DS="model.ra.redo", p=p )    
      debug=F
      if ( debug ) {
        Qa = predict.discretised.habitat( DS="model.ra", p=p )    
        summary(Qa) 
        plot(Qa, all.terms=T, rug=T, jit=T, seWithMean=T, trans=exp, pers=F , scale=0)
        vis.gam( Qa, view=c("plon", "plat"), plot.type="contour", n.grid=200, too.far=2 )
      }
    }
   
  }


      
  
  for ( sp in speciesofinterest ) {
  
    # sp = speciesofinterest = "wolffish"
    p$speciesofinterest = sp
    print(sp)

    # --------------
    # predict data: gridded extrapolations to full domain  
    p = make.list( list( y=p$yearstomodel), Y=p )
    parallel.run( clusters=p$clusters, n=p$nruns,  predict.discretised.habitat, DS="sim.redo", p=p ) 

      
    p = make.list( list(y=p$yearstomodel ), Y=p )
    parallel.run( clusters=p$clusters, n=p$nruns,  predict.discretised.habitat, DS="map.habitat", p=p ) 

  }  # end for each species






