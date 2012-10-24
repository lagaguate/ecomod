# ----------------------------------------------------------------------------------
# NOTE : 
#  
#  1. The year in the file "current.assessment.year.r" must be changed every year.
#     It must be kept separate from "load.snowcrab.environment" as running in parallel mode 
#     requires occasionally overrding some parameters in "p". This override cannot be completed as  
#     "load.snowcrab.environment.r" is sourced with every initialisation of a a new CPU.
# ----------------------------------------------------------------------------------
   
# load required functions and parameters 
#if (.Platform$OS.type=="windows")  {
#        project.directory("snowcrab") =  file.path( "J:", "projects", "snowcrab" )
#    }
	
	loadfunctions( "snowcrab", functionname="initialise.local.environment.r") 
  
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
    logbook.db( DS="odbc.logbook.redo", yrs=1996:p$current.assessment.year ) 
    logbook.db( DS="odbc.licence.redo" ) 
    logbook.db( DS="odbc.areas.redo" ) 
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

    # bring in the raw data for netmind and minilogs: incremental or a sequence of years
    # in 2004, new BIO data streams began: Assume historical data are correct
    yToload = 1999:p$current.assessment.year
   
    # bring in minilog and netmind data -- slow
    # could also just add the new year Y=p$current.assessment.year -- faster 
    # but prefer to refresh everything in case of gremlins
    minilog.db( DS="load", Y=yToload ) # minilog data series "begins" in 1999 -- 60 min?
    netmind.db( DS="load", Y=yToload ) # netmind data series "begins" in 1998 -- 60 min?

    # creates initial rdata and sqlite db
    snowcrab.db( DS="setInitial.redo", p=p )  
    
    # merge
    minilog.db( DS="set.minilog.lookuptable", Y=yToload )  
    minilog.db( DS="stats.redo", Y=yToload ) # ~ 2hr for 1999 to 2010 
       
    netmind.db( DS="set.netmind.lookuptable", Y=yToload )
    netmind.db( DS="stats.redo", Y=yToload ) # requires minilog stats .. do last ~ 6 hrs

    snowcrab.db( DS="set.clean.redo", proj.type=p$internal.projection )
    
    snowcrab.db( DS="det.initial.redo", p=p )
    snowcrab.db( DS="det.georeferenced.redo" ) 
    
    snowcrab.db( DS="cat.initial.redo", p=p )
    snowcrab.db( DS="cat.georeferenced.redo" )
    
    snowcrab.db( DS="set.merge.det.redo" )
    snowcrab.db( DS="set.merge.cat.redo" )  

   }

# -------------------------------------------------------------------------------------
# Bathymetry data
# must run sequence in ~/projects/bathymetry/src/bathymetry.r 
# only if updating bathymetry data ( e.g., add snow crab depth data )


# -----------------------------------------------------------------------------------
# Substrate type
# do not update unless substrate data has been changed ... see ~/projects/substrate/src/substrate.r

# -------------------------------------------------------------------------------------
# Groundfish data 
#  required also for updates to temps and species
# ... see ~/projects/groundfish/src/groundfish.r -- require sm.base and gs.hydro



# -------------------------------------------------------------------------------------
# Run BIO.DB to update the multi-survey databases /home/jae/projects/bio/src/bio.r
# -------------------------------------------------------------------------------------
  loadfunctions ("bio" )
	loadfunctions ( "bio", functionname="bio.r" )  # glue everything together


# -------------------------------------------------------------------------------------
# Temperature data
# must manually obtain data from the Biochem web site and 
# run the sequence in ~/projects/temperature/src/temperature.r


# -------------------------------------------------------------------------------------
# Update the taxonomy database .. esp after groundfishdata refresh
# see: ~/projects/taxonomy/src/taxonomy.r


# -------------------------------------------------------------------------------------
# Species area data
# must manually update the species area database in ~/projects/speciesarea/src/speciesarea.r
  
# -------------------------------------------------------------------------------------
# Species composition data
# must manually update the species area database in ~/projects/speciescomposition/src/speciescomposition.r
  
# -------------------------------------------------------------------------------------
# Sizespectrum data
# must manually update the species area database in ~/projects/sizespectrum/src/sizespectrum.r
 

# -------------------------------------------------------------------------------------
# Metabolism data
# must manually update the species area database in ~/projects/metabolism/src/metabolism.r


# -------------------------------------------------------------------------------------
# Habitat data 
# Glue all the above together in planar coord system to allow fast lookup of data for matching with set, logbook data
# run sequence in /home/jae/projects/habitat/src/habitat.r

# -------------------------------------------------------------------------------------
# final data lookup/matching .. AFTER refreshing all above tables (where relevent/possible)
  logbook.db( DS="fisheries.complete.redo", p=p )  
  snowcrab.db( DS ="set.complete.redo", p=p )   
  snowcrab.db( DS ="set.logbook.redo", p=p, yrs=1996:p$current.assessment.year ) # add gridded fisheries data
  
  make.timeseries.data(p=p, areas=p$regions )  #  timeseries of means of all survey data
  #  tsdata = snowcrab.db("set.timerseries")

# create a new lookuptable for data transformations after refreshing set data/ranges
  REPOS = recode.variable.initiate.db ( db="snowcrab" )

# snow crab found in external databases tapped into for habitat determination
  for ( vs in c( "R0.mass", "male.large", "male.small", "female.large", "female.small" ) ) {
    -------- not yet finished this one ... 
    snowcrab.external.db(p=p, DS="set.snowcrab.in.groundfish.survey.redo", vname=vs )
  }



# simple geometric means of raw data:  used by indicators ordination and some figures
  # takes many hours ... need to make parallel 
  tsdata =  get.time.series ( x=snowcrab.db( DS="set.logbook", p=p, yrs=1996:p$current.assessment.year ),
    regions=p$regions, vars=get.variables("all.data"), from.file=F, trim=0 )




  #  ----- experimental / tests

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
  p = spatial.parameters( type="snowcrab" )
	xyz=bathymetry.db(p=p, DS="baseline.planar.500")
  map( xyz, xyz.coords="planar", cfa.regions=T, depthcontours=T, pts=NULL, annot=NULL, fn="test", loc=getwd() )



