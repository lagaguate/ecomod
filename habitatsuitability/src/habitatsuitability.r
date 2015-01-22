
  # Habitat suitability estimation begins 
   
  
  # this depends upon the bio.db -- make sure it is up to date:
  # loadfunctions( "bio", functionname="bio.r" )



	p = list()
  p$init.files = loadfunctions(c( "spatialmethods", "utility", "parallel", "taxonomy", "bio", "habitat", "habitatsuitability", "temperature",  "bathymetry"	)) 
	p$libs = RLibrary (c("chron", "fields", "rgdal", "snow", "mgcv", "arm" ))
  
  p = spatial.parameters(p=p, type="SSE" ) # 4VWX
  p$studyarea = c( "4vwx" )
  # p$studyarea = c("4vwx", "5yz" )
  
   # set map years separately to temporal.interpolation.redo allow control over specific years updated
  p$yearstomodel = 1970:2014 
  # p$seasons = "allseasons"
 
  p$depthrange = c(10,700) # in meters ... allowable values of depth
  # p$temperaturerange = c() 

  p$interpolation.distances=c(1, 5, 10)
  
  
  # ---------
  # *** this is required *** 
  #
  # for choosing the right bio.db snapshot  ----
  # p$seasons = "allseasons"
	p$data.sources = c("groundfish", "snowcrab")  # absence defaults to "use all available data"
  p$taxa = "maxresolved"  
  # ---------



  # --------------
  # do spatial predictions using GAM

  debug = TRUE;  # debug = FALSE
  p$nsims = 1000;  if (debug) p$nsims = 100
  
  
  p$habitat.threshold.quantile = 0.05 # quantile at which to consider zero-valued abundance
  p$prediction.month = 7  # July
  
  p$optimizers = c( "nlm", "perf" )  # used by GAM

  # modeltype choice
  p$modeltype="simple"
  p$gam.model.pa = habitat.model.formula ( modeltype=p$modeltype )





  # database creation

  # must generate or regenerate the correct data selection
  initialize.database = FALSE
  if (initialize.database) {
    habitatsuitability.db( DS="initial.set.redo", p=p )  
  }



  p$speciesofinterest = "snowcrab"
  
  p$subset = "snowcrab.female.large"  # see lookup.biological.filter for predefined types
  # p$subset = "female" # p$subset = "female.immature" , etc


  habitatsuitability.db( DS="taxasubset.cat.redo", p=p ) 
  habitatsuitability.db( DS="taxasubset.det.redo", p=p ) 
  


  # example extraction  x = habitatsuitability.db( DS="subset", p=p ) 

 
  # choose:
  p$clusters = rep( "localhost", 1)  
  # p$clusters = rep( "localhost", 2 )
  # p$clusters = rep( "localhost", 8 )
  # p$clusters = rep( "localhost", 24 )
  # p$clusters = c( rep( "nyx.beowulf", 4), rep("tartarus.beowulf", 4), rep("kaos", 4 ) )


  p$taxalist = c( "snowcrab.f.small", "snowcrab.f.large" , "snowcrab.m.small" ,"snowcrab.m.large" )
  # p$taxalist = c( "wolffish", "white.hake", "thornyskate", "american.plaice", "cod", "redfish" )
  
  # --------------
  # data


  if (refresh.databases) {
    for (sp in taxalist ) {
      print (sp )
      p$speciesofinterest = sp
      habitatsuitability.db( DS="complete.redo", p=p )
    }
  }




  #----------------------------
  # model presence and absence using GAM

  for ( sp in speciesofinterest)  {
  
    # sp = speciesofinterest = "wolffish"
    # sp = speciesofinterest = "snowcrab.female.large"
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
    parallel.run( predict.discretised.habitat, DS="sim.redo", p=p ) 

      
    p = make.list( list(y=p$yearstomodel ), Y=p )
    parallel.run( predict.discretised.habitat, DS="map.habitat", p=p ) 

  }  # end for each species

