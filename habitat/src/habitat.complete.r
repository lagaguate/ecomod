 # -------------------------
  # create required annual datasets for predictions of habitat and abundance
  # ultimately to speed up processing by creating merged data that are repeatedly used by many other functions/analysis
  #  .. baseline prediction surface in planar coords
  #  .. a glue function to bring in all available temperature and biological and substrate information

  # requires speciesarea, specescomposition, bathy, temp, etc ...

  # This step needs to be completed after all other incoming db are refreshed
  
 
  ## i.e., DO NOT RUN THE REMAINDER 
  ## Until bio.db and all other higher level indicators have been completed.
  ## loadfunctions ( "bio", functionname="bio.r" )  
  ## loadfunctions ( "speciesarea", functionname="speciesarea.r" ) 
  ## loadfunctions ( "speciescomposition", functionname="speciescomposition.r" ) 
  ## loadfunctions ( "sizespectrum", functionname="sizespectrum.r" ) 
  ## loadfunctions ( "metabolism", functionname="metabolism.r" ) 
  ## etc, ...



    loadfunctions ( "habitat", functionname="habitat.parameters.r" ) 


 
     # add biologicals 
     
     # TODO :: biologicals begin in 1970 ..  need to fix 
     #        .. at present data from 1970 are copied to all pre 1970 data years
 
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

