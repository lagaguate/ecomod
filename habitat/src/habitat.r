
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
  p$init.files = loadfunctions( c(
	  "common", "habitat", "substrate", "bathymetry", "speciesarea", "metabolism", 
		"sizespectrum", "speciescomposition", "temperature", "biochem", "condition" 
	) )

  p$libs = loadlibraries( "mgcv", "sp", "gstat",  "parallel", "fields", "chron" ) 
  p$taxa = "maxresolved"
  p$season = "allseasons"
  p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 ) 
  p$interpolation.nmax = 100 
   
 
  p$yearstomodel = 1970:2013
  
  
  p$clusters = rep("localhost", detectCores() )
 # p$clusters = rep("localhost",8)
  # p$clusters = c( rep("kaos", 14), rep("tartarus", 14), rep("nyx", 14) )


  baseline.redo = FALSE
  if (baseline.redo) { 
    # physical characteristics (depth, temp, substrate)
    # Time-invariant data (depth, substate, etc) 
    p = spatial.parameters( p=p, type="SSE" )
    habitat.db( DS="baseline.redo", p=p ) ## Time-invariant data (depth, substate, etc) 
    lut = habitat.xyz.to.grid ( p, redo=TRUE ) # redo lookup table to convert xyz data to matrix/grid format
  }


  if (exists(habitat.add.environmentals) && habitat.add.environmentals ) {
    # Contains all environmental data == baseline and temperature data ... none of the 'higher level indicators'
    # Used for merging back into bio.db as the 'higher level indicators have not yet been created/updated
 		p = spatial.parameters( p=p, type="SSE" )
    p = make.list( list( yrs=p$yearstomodel), Y=p )
    parallel.run( habitat.db, DS="environmentals.redo", p=p )

  }



  ## DO NOT RUN THE REMAINDER 
  ## Until bio.db and all other higher level indicators have been completed.
  ## loadfunctions ( "bio", functionname="bio.r" )  
  ## loadfunctions ( "speciesarea", functionname="speciesarea.r" ) 
  ## loadfunctions ( "speciescomposition", functionname="speciescomposition.r" ) 
  ## loadfunctions ( "sizespectrum", functionname="sizespectrum.r" ) 
  ## loadfunctions ( "metabolism", functionname="metabolism.r" ) 
  ## etc, ...

  # --------------------

  if ( exists(habitat.add.biologicals) && habitat.add.biologicals ) {

    # add biologicals created in 
    # biologicals begin in 1970  # need to fix this but how? 
    # .. at present data from 1970 are copied to all pre 1970 data years

    p$speciesarea.modeltype = "complex"
    p$speciesarea.method = "glm"   ## this is chosen in speciesarea.r ... make sure it matches up
    p$speciesarea.season = "allseasons"  
    p$speciesarea.taxa = "maxresolved"  # use only unique taxa
    p$speciesarea.data.sources = c("groundfish", "snowcrab")
    p$speciesarea.variables = c( "C", "Z", "T", "Npred" )
    
    p$speciescomposition.modeltype = "complex"  
    p$speciescomposition.season = "allseasons"  
    p$speciescomposition.taxa = "maxresolved"  
    p$speciescomposition.variables = c( "ca1", "ca2" )

    p$sizespectrum.modeltype = "complex" 
    p$sizespectrum.taxa = "maxresolved"
    p$sizespectrum.season = "allseasons"
    p$sizespectrum.variables = c( "nss.b1", "nss.rsquared", "nss.shannon")

    p$condition.modeltype = "complex" 
    p$condition.taxa = "maxresolved"
    p$condition.season = "allseasons"
    p$condition.variables = c( "coAll", "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic", 
                               "coSmallPelagic", "coLargePelagic", "coSmallDemersal", "coLargeDemersal")

    p$metabolism.modeltype = "complex" 
    p$metabolism.taxa = "alltaxa"
    p$metabolism.season = "allseasons"
    p$metabolism.variables = c( "smr", "Pr.Reaction" , "Ea", "A", "qn", "qm", "mass", "len"  )

    
    p = spatial.parameters( p=p, type="SSE" )
    p = make.list( list( yrs=p$yearstomodel), Y=p )
    
    parallel.run(  habitat.db, DS="complete.redo", p=p )
    
  }


