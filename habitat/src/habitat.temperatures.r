
  # -------------------------
  # create required annual datasets for predictions of habitat and abundance
  # ultimately to speed up processing by creating merged data that are repeatedly used by many other functions/analysis
  #  .. baseline prediction surface in planar coords
  #  .. a glue function to bring in all available temperature and biological and substrate information

  # requires speciesarea, specescomposition, bathy, temp, etc ...

  # This step needs to be completed after temperature db refresh
  
    loadfunctions ( "habitat", functionname="habitat.parameters.r" ) 
 
    p$yearstomodel = 1970:2015
    
    # physical characteristics (depth, temp, substrate)
    # Time-invariant data (depth, substate, etc) 
    habitat.db( DS="baseline.redo", p=p ) ## Time-invariant data (depth, substate, etc) 
    lut = habitat.xyz.to.grid ( p, redo=TRUE ) # redo lookup table to convert xyz data to matrix/grid format
  
    # Contains all environmental data == baseline and temperature data ... none of the 'higher level indicators'
    # Used for merging back into bio.db as the 'higher level indicators have not yet been created/updated
    p = make.list( list( yrs=p$yearstomodel), Y=p )
    parallel.run( habitat.db, DS="environmentals.redo", p=p )




