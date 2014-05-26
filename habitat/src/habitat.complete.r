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



