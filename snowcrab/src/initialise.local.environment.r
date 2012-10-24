 

  # ----------------------------------------------------------------------------------
  # NOTE to all: The year of "current.assessment.year must be changed every year before any other run
  #       It cannot be automatically loaded together with the "load.snowcrab.environment". This is because 
  #       running in parallel mode requires overrding some parameters in "p" on occasion which cannot be done 
  #       ("load.snowcrab.environment" is sourced with every initialisation of a a new CPU).
  #       Copying the following into each relevent file is not a solution as it is error prone and  repetitive. 
  # ----------------------------------------------------------------------------------
   
  	loadfunctions( "snowcrab", functionname="current.assessment.year.r") 
		loadfunctions( "snowcrab", functionname="default.project.environment.r") 
	  
    p = get.parameters ( current.assessment.year=current.assessment.year, set="kriging.params")

  
