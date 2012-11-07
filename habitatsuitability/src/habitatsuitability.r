
  # Habitat suitability estimation template 
   
  ### define parameter list to determine the correct category of taxa/size from "bio.db" 


	loadlibraries (c("chron", "fields", "rgdal", "snow", "mgcv", "arm" ))


	p = list()
  p$env.init = loadfunctions(c( "common", "taxonomy", "bio", "habitat", "habitatsuitability", "temperature",  "bathymetry"	)) 
  
  p = spatial.parameters( type="SSE" ) # 4VWX
  p$studyarea = c( "4vwx" )
  # p$studyarea = c("4vwx", "5yz" )
  
  # set map years separately to temporal.interpolation.redo allow control over specific years updated
  p$yearstomodel = 1970:2011 
  # p$seasons = "allseasons"
  
  p$taxa =  "maxresolved"
  # p$taxa.secondary.filter = "" 

  # --------------
  # do spatial predictions using GAM

  debug = TRUE;  # debug = FALSE
  p$nsims = 1000;  if (debug) p$nsims = 100
  p$habitat.threshold.quantile = 0.05 # quantile at which to consider zero-valued abundance
  p$prediction.month = 7  # July
  
  p$optimizers = c( "nlm", "perf" )  # used by GAM

  # modeltype choice
  p$modeltype="simple"
  p$gam.model.pa = habitat.model.lookup (p$modeltype)



  # database creation
  p$subset = "snowcrab.female.large"  # also used as a label
	p$data.sources = c("groundfish", "snowcrab") 
  
    update.bio = FALSE # Remember to update BIO data if not already up to date
    if (update.bio) loadfunctions( "bio", functionname="bio.r" )


  # must generate or regenerate the correct data selection
  update.local.subset = FALSE
  if (update.local.subset) habitatsuitability.db( DS="bio.subset.redo", p=p ) 

  # example extraction  x = bio.db( DS="subset", p=p ) 

 
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

