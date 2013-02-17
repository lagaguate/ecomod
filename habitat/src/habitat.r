
  # -------------------------
  # create required annual datasets for predictions of habitat and abundance
  # ultimately to speed up processing by creating merged data that are repeatedly used by many other functions/analysis
  #  .. baseline prediction surface in planar coords
  #  .. a glue function to bring in all available temperature and biological and substrate information

  # requires speciesarea, specescomposition, bathy, temp, etc ...


#  require(car)
#  require(effects)  
#  require(lattice)
#  require(arm)
#  require(splines)
#  require (geoR)

  p = list()
  p$env.init = loadfunctions( c(
	  "common", "habitat", "substrate", "bathymetry", "speciesarea", "metabolism", 
		"sizespectrum", "speciescomposition", "temperature" 
	) )


  baseline.redo = FALSE
  if (baseline.redo) { 
    # physical characteristics (depth, temp, substrate)
    # Time-invariant data (depth, substate, etc) 
 	
    for ( j in c( "SSE") ) {  # sse is the only relevent area for which all data exists 
      p = spatial.parameters( p=p, type=j )
      p$taxa = "maxresolved"
      p$season = "allseasons"
      p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 ) 
      p$interpolation.nmax = 100 
      habitat.db( DS="baseline.redo", p=p ) ## Time-invariant data (depth, substate, etc) 
    }

  }


  # --------------------

  # add biologicals
  p$yearstomodel = 1970:2012
  # biologicals begin in 1970  # need to fix this but how? 
  # .. at present data from 1970 are copied to all pre 1970 data years
 

  p$speciesarea.modeltype = "complex"
  p$speciesarea.method = "glm"   ## this is chosen in speciesarea.r ... make sure it matches up
  p$speciesarea.season = "allseasons"  
  p$speciesarea.taxa = "maxresolved"  # use only unique taxa
  p$speciesarea.data.sources = c("groundfish", "snowcrab")

  p$speciescomposition.modeltype = "complex"  
  p$speciescomposition.season = "allseasons"  
  p$speciescomposition.taxa = "maxresolved"  
  
  p$sizespectrum.modeltype = "complex" 
  p$sizespectrum.taxa = "maxresolved"
  p$sizespectrum.season = "allseasons"

  p$metabolism.modeltype = "complex" 
  p$metabolism.taxa = "alltaxa"
  p$metabolism.season = "allseasons"

	
  for ( j in c( "SSE") ) {  # sse is the only relevent area for which all data exists ~ 5 min / yr 
		p = spatial.parameters( p=p, type=j )
		p$taxa = "maxresolved"
    p$season = "allseasons"
    p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 ) 
    p$interpolation.nmax = 100 
    
   
    habitat.db( DS="complete.redo", p=p ) 
    # or
    # p$clusters = rep("localhost",8)
    # p$clusters = c( rep("kaos", 14), rep("tartarus", 14), rep("nyx", 14) )
    # parallel.run(  clusters=p$clusters, n=length(p$yearstomodel), habitat.db, DS="complete.redo", p=p )
  }


