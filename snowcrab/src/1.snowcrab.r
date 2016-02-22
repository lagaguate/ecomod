# ----------------------------------------------------------------------------------
# NOTE : 
#  
#  1. The year in the file "current.assessment.year.r" must be changed every year.
#     It must be kept separate from "initialise.local.environment.r" as running in parallel mode 
#     requires occasionally overrding some parameters in "p". This override cannot be completed as  
#     "initialise.local.environment.r" is sourced with every initialisation of a a new CPU.
  # ----------------------------------------------------------------------------------
     
  # load required functions and parameters 
  	
  loadfunctions( "snowcrab", functionname="initialise.local.environment.r") 
    
  debug = FALSE
  if (debug) {
    p$do.parallel =F
    p$clusters= c("localhost")
  }

    # get data tables from Oracle server and store local copies
  # !!!!!! --------- these should be run on a windows machine: !!!!!!!!! <--------- READ THIS
  if (obtain.database.snapshot) {
    snowcrab.db( DS="set.odbc.redo", yrs=1996:p$current.assessment.year ) # Copy over datadirectory ("snowcrab"), "data", "trawl", "SNCRABSETS"
    snowcrab.db( DS="det.odbc.redo", yrs=1996:p$current.assessment.year ) # Copy over datadirectory ("snowcrab"), "data", "trawl", "SNCRABDETAILS"
    snowcrab.db( DS="cat.odbc.redo", yrs=1996:p$current.assessment.year ) # Copy over datadirectory ("snowcrab"), "data", "trawl", "SNTRAWLBYCATCH"
    logbook.db(  DS="odbc.logbook.redo", yrs=1996:p$current.assessment.year ) #Copy over datadirectory ("snowcrab"), "data", "logbook", "datadump"
    logbook.db(  DS="odbc.licence.redo" ) #Copy over datadirectory ("snowcrab"), "data", "logbook", "lic.datadump.rdata" 
    logbook.db(  DS="odbc.areas.redo" ) #Copy over datadirectory ("snowcrab"), "data", "observer", "datadump"
    observer.db( DS="odbc.redo", yrs=1996:p$current.assessment.year )  
  }

# -------------------------------------------------------------------------------------
# produce base data files from snowcrab logbook database (marfis) and historical data
# and the observer databases: 
# this needs to be done after the above datadumps to refresh locally stored databases

  if (make.fisheries.data) {
    observer.db( DS="odb.redo", p=p ) # 3 minutes
    # fishing ground are used for determination of contraints for interpolation
    logbook.db( DS="logbook.redo", p=p )
    logbook.db( DS="logbook.filtered.positions.redo", p=p )
    logbook.db( DS="fishing.grounds.redo",  p=p )
    logbook.db( DS="logbook.gridded.redo", p=p )
  }


# -------------------------------------------------------------------------------------
# create base set data and add all historical data fixes
  loadfunctions( "snowcrab", functionname="initialise.local.environment.r") 
   
  if (get.base.data) {
    # sequence is important ... do not change 
    # creates initial rdata and sqlite db
    snowcrab.db( DS="setInitial.redo", p=p ) # this is required by the seabird.db (but not minilog and netmind) 
    
    seabird.yToload = p$current.assessment.year
    minilog.yToload = p$current.assessment.year
    netmind.yToload = p$current.assessment.year
    esonar.yToload  = p$current.assessment.year
 
    if (redo.all.data) {
      seabird.yToload = 2012:p$current.assessment.year
      minilog.yToload = 1999:p$current.assessment.year
      netmind.yToload = 1999:p$current.assessment.year
      esonar.yToload  = 2014:p$current.assessment.year
    }

    seabird.db( DS="load", Y=seabird.yToload ) # this begins 2012;
    minilog.db( DS="load", Y=minilog.yToload ) # minilog data series "begins" in 1999 -- 60 min?
    
    netmind.db( DS='esonar2netmind.conversion',Y=esonar.yToload ) 
    netmind.db( DS="load", Y=netmind.yToload) # netmind data series "begins" in 1998 -- 60 min?

    #MG I'm not sure why these stats are not being written automatically, neet to set it in the code above to run these after data is loaded
    seabird.db (DS="stats.redo", Y=seabird.yToload )
    minilog.db (DS="stats.redo", Y=minilog.yToload )
    netmind.db (DS="stats.redo", Y=netmind.yToload )
   
   
    set <- snowcrab.db( DS="setInitial", p=p ) # this is required by the seabird.db (but not minilog and netmind) 
    # set2015 <- set[which(set$yr == '2015'),] #check to make sure 2015 data is in there properly
    # head(set2015)  
      
      problems = data.quality.check( set, type="stations")     
      problems = data.quality.check( set, type="count.stations")
      problems = data.quality.check( set, type="position") 
      #MG try checking the end position of the tow, if there is an error
      
      problems = data.quality.check( set, type="minilog.mismatches" )
      problems = data.quality.check( set, type="tminilog.load")
      problems = data.quality.check( set, type="minilog.dateproblems") 
      problems = data.quality.check( set, type="minilog") # Check for duplicate timestamps 
      
      problems = data.quality.check( set, type="netmind.load")
      problems = data.quality.check( set, type="netmind.mismatches" )

      problems = data.quality.check( set, type="tow.duration")
      problems = data.quality.check( set, type="tow.distance")
      
      problems = data.quality.check( set, type="seabird.mismatches" )
      problems = data.quality.check( set, type="seabird.load") 
      
      problems = data.quality.check( set, type="netmind.timestamp" )


    snowcrab.db( DS="set.clean.redo", p=p )  # sanity checks
    #MG det.initial.redo updates and processes morphology. This code now identifies morphology errors, which must be
    #checked with written logs, then sent to database and put in debugging here and re-run
    snowcrab.db( DS="det.initial.redo", p=p )
    snowcrab.db( DS="det.georeferenced.redo" ) 
    snowcrab.db( DS="cat.initial.redo", p=p )
    snowcrab.db( DS="cat.georeferenced.redo" )
    snowcrab.db( DS="set.merge.det.redo" )
    snowcrab.db( DS="set.merge.cat.redo" )  
  }  # end base data
  
  
  loadfunctions( "snowcrab", functionname="initialise.local.environment.r") 
  parameters.initial = p  # copy here as the other calls below overwrites p# -------------------------------------------------------------------------------------
# External Dependencies: (must be completed before the final lookup/mathcing phase)
#     Bathymetry data :: 
  loadfunctions("bathymetry", functionname="bathymetry.r" ) # if necessary
#     Substrate type  :: 
  loadfunctions("substrate", functionname="substrate.r" ) # if necessary
#     Groundfish data :: 
#     NOTE  groundfish.db( DS="odbc.redo" ) must first be done manually 
#     on a windows machine and data snapshots moved to local system
  loadfunctions( "groundfish", functionname="1.groundfish.r" ) #MG took 15-25 minutes to run
#     Taxonomy :: 
  loadfunctions("taxonomy", functionname="taxonomy.r" ) # if necessary #MG takes about 30 minutes to run
## The following are very SLOW: 
# Temperatures ::  
  loadfunctions ( "temperature", functionname="temperature.r" )  # days to run
# Habitat data ... environmentals only as it is used by bio.db etc
  loadfunctions ( "habitat", functionname="habitat.r") #MG fairly quick to run
  #loadfunctions ( "habitat", functionname="habitat.temperatures.r" ) 
# BIO db update :: 
# must come after temperature interpolations to permit temperature lookups 
  loadfunctions ( "bio", functionname="bio.r" )  #MG took about 20 minutes to run
# the folllowing depends upon bio.db and temperature  
  #MG species area took 2 days to run in parallel, run some things on server if possible. It's quicker to run some things in serial though, ask Jae
  loadfunctions ( "speciesarea", functionname="speciesarea.r" ) 
  loadfunctions ( "speciescomposition", functionname="speciescomposition.r" ) 
  loadfunctions ( "sizespectrum", functionname="sizespectrum.r" ) 
  loadfunctions ( "metabolism", functionname="metabolism.r" ) 
  loadfunctions ( "condition", functionname="condition.r" ) #MG this one took 8 days to run on the laptop, in serial...
# Habitat data :: NOTE:: This glues all the above together in planar coord system 
# to allow fast lookup of data for matching with set, logbook data
  loadfunctions ( "habitat", functionname="habitat.complete.r" ) 

# -------------------------------------------------------------------------------------
# Final data lookup/matching .. AFTER refreshing all above tables (where relevent/possible)
  
  p = parameters.initial
  
  logbook.db( DS  ="fisheries.complete.redo", p=p )  
  snowcrab.db( DS ="set.complete.redo", p=p )   
  snowcrab.db( DS ="set.logbook.redo", yrs=1996:p$current.assessment.year ) # add gridded fisheries data
  # snowcrab.db( DS ="set.logbook", yrs=1996:p$current.assessment.year ) 
  
  #make.timeseries.data(p=p, areas=p$regions )  #  timeseries of means of all survey data
  #in 2014 as there were reduced stations for comparison
  #make.timeseries.data(p=p, areas=p$regions,reduced.stations=F, vars='R0.mass' ) #  timeseries of means of all survey data
  make.timeseries.data(p=p, areas=NULL,reduced.stations=F, vars=NULL) #  timeseries of means of all survey data
  #make.timeseries.data(p=p, areas=NULL,reduced.stations=F, vars=c('ms.mass.10', 'ms.mass.30', 'ms.mass.201', 'ms.mass.50', 'ms.mass.2521', 'ms.mass.2511', 'ms.mass.202', 'ms.mass.204', 'ms.mass.2211'), outfile = file.path( project.datadirectory("snowcrab"), "R", "tsbycatch.rdata" )) #  timeseries of means of all survey data


  #  tsdata = snowcrab.db("set.timerseries")

# create a new lookuptable for data transformations after refreshing set data/ranges
  REPOS = recode.variable.initiate.db ( db="snowcrab" )


# snow crab found in external databases tapped into for habitat determination
  #for ( vs in c( "R0.mass", "male.large", "male.small", "female.large", "female.small" ) ) {
    ### -------- not yet finished this one ...  TODO
    vs="R0.mass"
    snowcrab.external.db(p=p, DS="set.snowcrab.in.groundfish.survey.redo", vname=vs )

    ---- TODO !!! must replace this with bio.db proceeing step
    
    #}



# simple geometric means of raw data:  used by indicators ordination and some figures
  # takes many hours ... need to make parallel  TODO 
  tsdata =  get.time.series ( x=snowcrab.db( DS="set.logbook"),
  regions=p$regions, vars=variable.list.expand("all.data"), from.file=F, trim=0 )



#  ----- experimental / tests / ideas
testing = F
if (testing) {
  s =  snowcrab.db( DS ="set.complete" )
  d =   snowcrab.db( DS ="det.georeferenced" ) 
  l = merge( d, s[, c("trip", "set", "t")], by=c("trip", "set"), all.x=T, all.y=F)
  rm(s,d); gc()
  l = l[ which( as.numeric(as.character(l$mat)) %in% c(mature, immature)  &
                l$sex %in% c(male, female) ) , ]
  l$sex[ which( l$sex ==male) ] = "male"
  l$sex[ which( l$sex ==female) ] = "female"

  l$sex= factor( as.character(l$sex) )
  l$mat = factor( as.character( l$mat))

  m = glm( t~ as.factor(mat) * as.factor(sex), data= l, family=gaussian())
  require(car)

  Anova( m)
  require(effects)
  k=all.effects(m, l)
  plot(k)


  # -------------------------------------------------------------------------------------
  # make size at maturity estimates in a spatial context

  if( make.maturity.db ) {
    maturity = make.maturity.spatial( distance=50 )
    save(maturity, file="maturity.rdata", compress=T)
    # load(file.path( project.datadirectory("snowcrab"), "snowcrab", "R", "maturity.rdata"))
  }



  # -------------------------------------------------------------------------------------
  # example plot mechanism
  p = spatial.parameters( p=p, type="snowcrab" )
	xyz=bathymetry.db(p=p, DS="baseline.planar.500")
  map( xyz, xyz.coords="planar", cfa.regions=T, depthcontours=T, pts=NULL, annot=NULL, fn="test", loc=getwd() )

} # end testing

