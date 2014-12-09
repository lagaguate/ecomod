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
    snowcrab.db( DS="set.odbc.redo", yrs=1996:p$current.assessment.year ) 
    snowcrab.db( DS="det.odbc.redo", yrs=1996:p$current.assessment.year ) 
    snowcrab.db( DS="cat.odbc.redo", yrs=1996:p$current.assessment.year ) 
    logbook.db(  DS="odbc.logbook.redo", yrs=1996:p$current.assessment.year ) 
    logbook.db(  DS="odbc.licence.redo" ) 
    logbook.db(  DS="odbc.areas.redo" ) 
    observer.db( DS="odbc.redo", yrs=1996:p$current.assessment.year )  
  }

# -------------------------------------------------------------------------------------
# produce base data files from snowcrab logbook database (marfis) and historical data
# and the observer databases: 
# this needs to be done after the above datadumps to refresh locally stored databases

  if (make.fisheries.data) {
    observer.db( DS="odb.redo", p=p )
    # fishing ground are used for determination of contraints for interpolation
    logbook.db( DS="logbook.redo", p=p )
    logbook.db( DS="logbook.filtered.positions.redo", p=p )
    logbook.db( DS="fishing.grounds.redo",  p=p )
    logbook.db( DS="logbook.gridded.redo", p=p )
  }


# -------------------------------------------------------------------------------------
# create base set data and add all historical data fixes

  if (get.base.data) {
    
    # sequence is important ... do not change 
    
    # creates initial rdata and sqlite db
    snowcrab.db( DS="setInitial.redo", p=p ) # this is required by the seabird.db (but not minilog and netmind) 
 
    # unless there are structural changes in approach, incremental update is fine
    seabird.yToload = p$current.assessment.year
    minilog.yToload = p$current.assessment.year
    netmind.yToload = p$current.assessment.year
      if (full.redo) {
        seabird.yToload = 2012:p$current.assessment.year
        minilog.yToload = 1999:p$current.assessment.year
        netmind.yToload = 1999:p$current.assessment.year
      }     
     

    # The following requires "setInitial"

    seabird.db( DS="load", Y=seabird.yToload ) # this begins 2012;
    minilog.db( DS="load", Y=minilog.yToload ) # minilog data series "begins" in 1999 -- 60 min?
    netmind.db( DS="load", Y=netmind.yToload) # netmind data series "begins" in 1998 -- 60 min?

    
    seabird.db( DS="stats.redo", Y=seabird.yToload ) # ~ 20 min
    minilog.db( DS="stats.redo", Y=minilog.yToload ) # ~ 5 min for 1999 to 2010 
    netmind.db( DS="stats.redo", Y=netmind.yToload ) # requires minilog and seabird stats .. do last ~ 30 min


    snowcrab.db( DS="set.clean.redo", proj.type=p$internal.projection )
    set <- snowcrab.db( DS="setInitial", p=p ) # this is required by the seabird.db (but not minilog and netmind) 

       
      problems = data.quality.check( set, type="stations")     
      problems = data.quality.check( set, type="count.stations")
      problems = data.quality.check( set, type="position") 
      
      problems = data.quality.check( set, type="minilog.mismatches" )
      problems = data.quality.check( set, type="minilog.load")
      problems = data.quality.check( set, type="minilog.dateproblems") 
      problems = data.quality.check( set, type="minilog") # Check for duplicate timestamps 
      
      problems = data.quality.check( set, type="netmind.load")
      problems = data.quality.check( set, type="netmind.mismatches" )
      
      problems = data.quality.check( set, type="tow.duration")
      problems = data.quality.check( set, type="tow.distance")
      
      problems = data.quality.check( set, type="seabird.mismatches" )
      problems = data.quality.check( set, type="seabird.load") 
      
      problems = data.quality.check( set, type="netmind.timestamp" )

    
    snowcrab.db( DS="det.initial.redo", p=p )
    snowcrab.db( DS="det.georeferenced.redo" ) 
    snowcrab.db( DS="cat.initial.redo", p=p )
    snowcrab.db( DS="cat.georeferenced.redo" )
    snowcrab.db( DS="set.merge.det.redo" )
    snowcrab.db( DS="set.merge.cat.redo" )  

  }  # end base data


  parameters.initial = p  # copy here as the other calls below overwrites p

# -------------------------------------------------------------------------------------
# External Dependencies: (must be completed before the final lookup/mathcing phase)

  
#     Bathymetry data :: 
  loadfunctions("bathymetry", functionname="bathymetry.r" ) # if necessary

#     Substrate type  :: 
  loadfunctions("substrate", functionname="substrate.r" ) # if necessary

#     Groundfish data :: 
#     NOTE  groundfish.db( DS="odbc.redo" ) must first be done manually 
#     on a windows machine and data snapshots moved to local system
  loadfunctions( "groundfish", functionname="1.groundfish.r" ) 

#     Taxonomy :: 
  loadfunctions("taxonomy", functionname="taxonomy.r" ) # if necessary


## The following are very SLOW: 
# Temperatures ::  
  loadfunctions ( "temperature", functionname="temperature.r" )  # days

  
# Habitat data ... environmentals only as it is used by bio.db etc
  loadfunctions ( "habitat", functionname="habitat.temperatures.r" ) 

# BIO db update :: 
# must come after temperature interpolations to permit temperature lookups 
  loadfunctions ( "bio", functionname="bio.r" )  

# the folllowing depends upon bio.db and temperature  
  loadfunctions ( "speciesarea", functionname="speciesarea.r" ) 
  loadfunctions ( "speciescomposition", functionname="speciescomposition.r" ) 
  loadfunctions ( "sizespectrum", functionname="sizespectrum.r" ) #ran into problems with mapping these need to check sept 7 2014 
  loadfunctions ( "metabolism", functionname="metabolism.r" ) 
  loadfunctions ( "condition", functionname="condition.r" ) 


# Habitat data :: NOTE:: This glues all the above together in planar coord system 
# to allow fast lookup of data for matching with set, logbook data
  loadfunctions ( "habitat", functionname="habitat.complete.r" ) 



# -------------------------------------------------------------------------------------
# Final data lookup/matching .. AFTER refreshing all above tables (where relevent/possible)
  
  p = parameters.initial
  
  logbook.db( DS="fisheries.complete.redo", p=p )  
  snowcrab.db( DS ="set.complete.redo", p=p )   
  snowcrab.db( DS ="set.logbook.redo", yrs=1996:p$current.assessment.year ) # add gridded fisheries data
  
  make.timeseries.data(p=p, areas=p$regions )  #  timeseries of means of all survey data
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
    # load(file.path( project.directory("snowcrab"), "snowcrab", "R", "maturity.rdata"))
  }



  # -------------------------------------------------------------------------------------
  # example plot mechanism
  p = spatial.parameters( p=p, type="snowcrab" )
	xyz=bathymetry.db(p=p, DS="baseline.planar.500")
  map( xyz, xyz.coords="planar", cfa.regions=T, depthcontours=T, pts=NULL, annot=NULL, fn="test", loc=getwd() )

} # end testing

