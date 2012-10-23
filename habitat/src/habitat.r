
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
  p$yearstomodel = 1970:2011
  # biologicals begin in 1970  # need to fix this but how? 
  # .. at present data from 1970 are copied to all pre 1970 data years
 


  # <<<<<<<------------ do not use complex models here as then would create a circular data loop

  p$speciesarea.modeltype = "simple.highdef"
  p$speciesarea.method = "glm"   ## this is chosen in speciesarea.r ... make sure it matches up
  p$speciesarea.season = "allseasons"  
  p$speciesarea.taxa = "maxresolved"  # use only unique taxa

  p$speciescomposition.modeltype = "simple.highdef"  
  p$speciescomposition.season = "allseasons"  
  p$speciescomposition.taxa = "maxresolved"  
  
  p$sizespectrum.modeltype = "simple.highdef" 
  p$sizespectrum.taxa = "maxresolved"
  p$sizespectrum.season = "allseasons"

  p$metabolism.modeltype = "simple.highdef" 
  p$metabolism.taxa = "alltaxa"
  p$metabolism.season = "allseasons"

	
  for ( j in c( "SSE") ) {  # sse is the only relevent area for which all data exists 
		p = spatial.parameters( p=p, type=j )
		p$taxa = "family.or.genera"
    p$season = "allseasons"
    p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 ) 
    p$interpolation.nmax = 100 
    
    # physical characteristics (depth, temp, substrate)
    habitat.db( DS="baseline.redo", p=p ) 
	
    # biologicals
    habitat.db( DS="complete.redo", p=p ) 
    # or
    # p$clusters = rep("localhost",8)
    # p$clusters = c( rep("kaos", 14), rep("tartarus", 14), rep("nyx", 14) )
    # parallel.run(  clusters=p$clusters, n=length(p$yearstomodel), habitat.db, DS="complete.redo", p=p )
  }


